import orgparse as op
from sklearn.compose import ColumnTransformer
from sklearn.feature_extraction.text import CountVectorizer
import sqlite3
import os
import re
import pandas as pd
import pickle
import json
from loguru import logger
from sentence_transformers import SentenceTransformer
from sklearn.base import BaseEstimator, TransformerMixin


class SentenceTransformerWrapper(BaseEstimator, TransformerMixin):
    def __init__(self):
        self.model = SentenceTransformer("all-MiniLM-L6-v2", backend="onnx")

    def fit(self, X, y=None):
        return self

    def transform(self, X):
        return self.model.encode(X.tolist())


def read_nodes(db_path: str) -> list:
    conn = sqlite3.connect(db_path)
    cur = conn.cursor()

    return [
        {
            "id": it[0].strip("\""),
            "file": it[1].strip("\""),
            "title": it[2].strip("\"")
        }
        for it in cur.execute("SELECT id, file, title FROM nodes").fetchall()
    ]


def parse_node(node: dict) -> dict:
    """Parse partially read node and populate more fields relevant for further
    processing.
    """

    with open(node["file"]) as fp:
        text = fp.read()

    match = re.search(r"^\#\+TAGS:\s*(.*?)\s*$", text, re.MULTILINE | re.IGNORECASE)

    tags = []
    if match:
        tags = [tag.strip() for tag in match.group(1).split(",")]

    return {
        "tags": tags,
        "content": op.loads(text).body,
        **node
    }


def clean_link_context(context: str) -> str:
    """Clean context string to remove org links.
    """

    pattern = r"\[\[id:[\w-]+\](?:\[(.*?)\])?\]"
    return re.sub(pattern, lambda m: m.group(1) if m.group(1) else "", context)


def is_context_null(context: str) -> bool:
    """Tell if this context is only the link itself. In that case, there is not
    much we can decorate the connection with.
    """

    return bool(re.match(r"^([\+\-\d]+\.? )?\[\[.+\](\[.+\])?\]$", context.strip()))


def featurize(webdump_dir: str, prep_file_path: str, features_output_path: str, db_path: str):
    # First we will featurize the nodes
    nodes = [parse_node(n) for n in read_nodes(db_path)]
    df = pd.DataFrame(nodes)
    df["tags"] = df["tags"].map(lambda ts: " ".join(ts))

    transformer = ColumnTransformer(
        transformers=[
            ("tags", CountVectorizer(), "tags"),
            ("title", SentenceTransformerWrapper(), "title")
        ],
        remainder="drop"
    )

    data = {
        "nodes": { "ids": df["id"].to_list(), "features": transformer.fit_transform(df) }
    }

    # Next we will featurize the links
    with open(prep_file_path) as fp:
        links = json.load(fp)["links"]
        logger.info(f"Loaded {len(links)} links")

    f_links = [link for link in links if not is_context_null(link["context"])]

    logger.info(f"{len(f_links)} links left after filtering.")

    model = SentenceTransformer("all-MiniLM-L6-v2", backend="onnx")
    link_features = model.encode([clean_link_context(link["context"]) for link in f_links])

    data["links"] = { "metadata": f_links, "features": link_features }

    with open(features_output_path, "wb") as fp:
        pickle.dump(data, fp)
