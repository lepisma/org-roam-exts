"""
Org-Roam Sem

`webdump` sub command dumps web links in a dump directory from all HTTP(S) type
ROAM_REFS. Output files are kept in separate .txt files with the ID of the
respective node as name. This is done separately so that the text files can be
indexed by recoll.

Usage:
  org-roam-sem prep <output-path> [--org-roam-db=<org-roam-db>]
  org-roam-sem prep webdump <output-path> [--org-roam-db=<org-roam-db>]
  org-roam-sem featurize <webdump-dir> <prep-file> <features-output> [--org-roam-db=<org-roam-db>]
  org-roam-sem serve <features-dump> [--org-roam-db=<org-roam-db>]

Options:
  --org-roam-db=<org-roam-db>    Path to SQLite roam db [default: ~/.emacs.d/org-roam.db]
"""

from docopt import docopt
from org_roam_sem.prep import prep
from org_roam_sem.featurize import featurize
from org_roam_sem.serve import serve

import os


def main():
    args = docopt(__doc__)
    db_path = os.path.expanduser(args["--org-roam-db"])

    if args["prep"]:
        prep(args["webdump"], os.path.expanduser(args["<output-path>"]), db_path)
    elif args["featurize"]:
        featurize(os.path.expanduser(args["<webdump-dir>"]), os.path.expanduser(args["<prep-file>"]), os.path.expanduser(args["<features-output>"]), db_path)
    elif args["serve"]:
        serve(os.path.expanduser(args["<features-dump>"]), db_path)