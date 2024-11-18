from glob import glob
import sqlite3
import os
from loguru import logger
from bs4 import BeautifulSoup
import requests
from tqdm import tqdm
from concurrent.futures import ThreadPoolExecutor, as_completed


def read_done_ids(dumpdir_path: str) -> list[str]:
    """Return list of node ids that have already been text dumped.
    """

    return [os.path.splitext(os.path.basename(file))[0] for file in glob(f"{dumpdir_path}/*.txt")]


def read_refs(db_path: str) -> list:
    conn = sqlite3.connect(db_path)

    def dict_factory(cursor, row):
        fields = [column[0] for column in cursor.description]
        return { key: value.strip("\"") for key, value in zip(fields, row) }

    conn.row_factory = dict_factory

    cur = conn.cursor()
    items = cur.execute("SELECT node_id, ref, type FROM refs").fetchall()

    return items


def build_url(ref: dict) -> str:
    return ref["type"] + ":" + ref["ref"]


def download_ref(ref: dict, output_dir: str):
    url = build_url(ref)
    output_name = ref["node_id"] + ".txt"
    output_path = os.path.join(output_dir, output_name)

    try:
        response = requests.get(url, headers={ "User-Agent": "Mozilla/5.0 (X11; Linux x86_64; rv:131.0) Gecko/20100101 Firefox/131.0" })
    except Exception as e:
        logger.warning(e)
        return

    if response.status_code != 200:
        logger.warning(f"Got {response.status_code} for {url}")
        return

    soup = BeautifulSoup(response.text, "html.parser")
    output = soup.get_text(separator=" ", strip=True)

    with open(output_path, "w") as fp:
        fp.write(output)


def prep(do_webdump: bool, output_path: str, db_path: str):
    if do_webdump:
        refs = read_refs(db_path)
        done_ids = set(read_done_ids(output_path))

        refs_todo = [ref for ref in refs if ref["node_id"] not in done_ids and ref["type"] in {"http", "https"}]
        logger.info(f"Downloading {len(refs_todo)} out of {len(refs)} URLs.")

        with ThreadPoolExecutor(max_workers=10) as executor:
            futures = { executor.submit(download_ref, ref, output_path): ref for ref in refs_todo }
            for future in tqdm(as_completed(futures), total=len(refs_todo)):
                future.result()
    else:
        # Run general preparation of all kinds and pack the output in a
        # single file.
        raise NotImplementedError()
