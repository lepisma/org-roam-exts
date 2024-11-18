import pickle
import sqlite3

from flask import Flask, jsonify, g
from sklearn.metrics.pairwise import cosine_similarity
from sentence_transformers import SentenceTransformer
from loguru import logger

app = Flask(__name__)


def get_similar_ids(node_id: str, blacklist_ids: set[str]) -> list[tuple[str, float]]:
    idx_and_scores = list(enumerate(app.config["data"]["node"]["similarities"][app.config["data"]["node"]["id_to_index"][node_id]]))
    idx_and_scores = sorted(idx_and_scores, key=lambda it: it[1], reverse=True)
    blacklist_idx = set([app.config["data"]["node"]["id_to_index"][bid] for bid in blacklist_ids])

    min_score = 0.6
    max_count = 10

    output = []
    for idx, score in idx_and_scores:
        if score < min_score:
            break
        if idx in blacklist_idx:
            continue
        if len(output) == max_count:
            break

        output.append((app.config["data"]["node"]["ids"][idx], score))

    return output


def get_connected_ids(node_id: str, db_path: str) -> set[str]:
    conn = sqlite3.connect(db_path)
    cur = conn.cursor()
    links = [(source.strip("\""), dest.strip("\"")) for (source, dest) in cur.execute("SELECT source, dest FROM links WHERE type = '\"id\"'").fetchall()]

    output = []
    for source, dest in links:
        if source == node_id:
            output.append(dest)
        elif dest == node_id:
            output.append(source)

    return set(output)


@app.route("/similar/<uuid>", methods=["GET"])
def similar(uuid: str):
    try:
        blacklist_ids = get_connected_ids(uuid, app.config["data"]["db_path"])
        blacklist_ids.add(uuid)

        similar_ids_with_scores = get_similar_ids(uuid, blacklist_ids)
        return jsonify(similar_ids_with_scores), 200
    except Exception as e:
        logger.error(e)
        return jsonify([]), 200


@app.route("/search/links/<query>", methods=["GET"])
def search_links(query: str):
    # TODO: This is an incorrect implementation, it can appear to be correct but
    #       is just used right now to test the system.
    X = app.config["data"]["embedder"].encode([query])
    idx_and_scores = list(enumerate(cosine_similarity(X, app.config["data"]["link"]["features"]).flatten()))
    idx_and_scores = sorted(idx_and_scores, key=lambda it: it[1], reverse=True)

    min_score = 0.3
    max_count = 10

    output = []
    for idx, score in idx_and_scores:
        if score < min_score:
            break
        if len(output) == max_count:
            break

        output.append((app.config["data"]["link"]["metadata"][idx], float(score)))

    return jsonify(output), 200


def serve(feature_dump_path: str, db_path: str):
    with open(feature_dump_path, "rb") as fp:
        data = pickle.load(fp)
        node_ids = data["nodes"]["ids"]

    app.config["data"] = {
        "db_path": db_path,
        "embedder": SentenceTransformer("all-MiniLM-L6-v2", backend="onnx"),
        "node": {
            "ids": node_ids,
            "id_to_index": { id: i for i, id in enumerate(node_ids) },
            "similarities": cosine_similarity(data["nodes"]["features"])
        },
        "link": {
            "features": data["links"]["features"],
            "metadata": data["links"]["metadata"]
        }
    }

    app.run(host="localhost", port=8431)
