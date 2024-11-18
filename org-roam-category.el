;;; org-roam-category.el --- Categorization functions for roam items -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Categorization functions for roam items
;; This file is not a part of GNU Emacs.

;;; License:

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program. If not, see <https://www.gnu.org/licenses/>.

;;; Code:

(require 'org-roam)

(cl-defmethod org-roam-node-recipe-p ((node org-roam-node))
  "Tell whether NODE is a recipe."
  (let* ((recipe-root-ids (list "a0699249-2b22-4e35-abdd-62c2c4bc9e16"
                                "39ba58d0-bf63-443d-afd3-e6b2bf86fbff"
                                "04a2283d-a4e8-4965-98a4-d2405b22716e"
                                "8951f4b8-88e8-479a-a859-1e746419795b"
                                "e7d62391-0251-46cf-abf3-10efb6f675c4"
                                "baaf2540-18bc-4a09-b94d-86b6325131af"
                                "6ffb65b3-ced4-453c-bc24-450ea4bb80e8"
                                "e996fc8b-1032-4642-82b0-5d7025b9bc3d"
                                "cdba62a5-3855-4f38-96b9-cab37744a3c4"
                                "95c12317-0372-4acb-80eb-b4b0451565bf"
                                "f7410330-5e6c-40fb-924c-3c1d5fe5dfd7"
                                "8a6c654f-5b06-4313-83af-8794006396fc"
                                "8a6c654f-5b06-4313-83af-8794006395fc"
                                "8bbd6914-57a4-4771-a1fe-f244e0bac00f"
                                "186f732e-d69c-4d75-88c5-e2c32a66a327"))
         (sql [:select [source dest] :from links :where (= dest $s1) :and (= type "id")])
         (link-pairs (org-roam-db-query sql (org-roam-node-id node))))
    (when link-pairs
      (catch 'found-match
        (dolist (link-pair link-pairs)
          (when (member (car link-pair) recipe-root-ids)
            (throw 'found-match t)))))))

(cl-defmethod org-roam-node-journal-p ((node org-roam-node))
  "Tell whether NODE is a Journal item."
  (when-let ((file (org-roam-node-file node)))
    (string= "daily" (car (last (butlast (file-name-split file)))))))

(cl-defmethod org-roam-node-non-filetags ((node org-roam-node))
  "Return tag for NODE based on regular TAGS directive and not
FILETAGS. This is because I haven't been using filetags for my
notes."
  (save-excursion
    (let ((file (org-roam-node-file node)))
      (if (not (and file (file-exists-p file)))
          (progn (warn "File doesn't exist for node-id: %s" (org-roam-node-id node))
                 nil)
        (with-current-buffer (find-file-noselect file)
          (goto-char (point-min))
          (when (re-search-forward "^#\\+TAGS:\\(.*\\)$" nil t)
            (mapcar #'string-trim (string-split (match-string-no-properties 1) ","))))))))

(cl-defmethod org-roam-node-category ((node org-roam-node))
  "Return category for given NODE. Right now it's based on explicit
tags given by me but would be automated later."
  (let ((tags  (org-roam-node-non-filetags node)))
    (cond
     ((member "research" tags) "Research Works")
     ((member "book" tags) "Books")
     ((member "project" tags) "Projects or Products")
     ((member "person" tags) "Person")
     ((member "unsorted" tags) "Unread Bookmarks")
     ((org-roam-node-recipe-p node) "Recipes")
     ((org-roam-node-journal-p node) "Journal Entries")
     ((org-roam-node-refs node) "Read Bookmarks")
     (t "Other Links"))))

(provide 'org-roam-category)

;;; org-roam-category.el ends here
