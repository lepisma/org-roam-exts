;;; org-roam-sem.el --- Semantic extensions for Org-Roam -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Semantic extensions for Org-Roam
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

(require 'cl-generic)
(require 'org-roam)
(require 'org-roam-category)
(require 'sem)
(require 'sem-embed)

(defcustom org-roam-sem-db-name "org-roam-sem"
  "Name of the sem database to use for storing vectors."
  :type 'string)

(defcustom org-roam-sem-nodes-table "nodes"
  "Name of the table where node embeddings are going to be stored."
  :type 'string)

(defcustom org-roam-sem-links-table "links"
  "Name of the table where link embeddings are going to be kept."
  :type 'string)

(defvar org-roam-sem-db nil
  "Placeholder for sem db.")

(defun org-roam-sem-setup ()
  "Connect to database, creating it and needed tables if not already
present."
  (setq org-roam-sem-db (if (sem-db-present-p org-roam-sem-db-name)
                            (sem-db-load org-roam-sem-db-name)
                          (sem-db-new org-roam-sem-db-name sem-embed-dim)))
  (unless (sem-table-present-p org-roam-sem-db org-roam-sem-nodes-table)
    (sem-table-new org-roam-sem-db org-roam-sem-nodes-table sem-embed-dim))
  (unless (sem-table-present-p org-roam-sem-db org-roam-sem-links-table)
    (sem-table-new org-roam-sem-db org-roam-sem-links-table sem-embed-dim)))

(defun org-roam-node-embed-batch (nodes)
  "Batch embed NODES.

At present this only returns sentence vector for the title and does not
use any other content like tags."
  (sem-embed-default (apply #'vector (mapcar #'org-roam-node-title nodes))))

(cl-defmethod org-roam-node-embed ((node org-roam-node))
  "Return a vector embedding for given NODE."
  (aref (org-roam-node-embed-batch (list node)) 0))

(cl-defmethod org-roam-node--sem-write-fn ((node org-roam-node))
  "The function to serialize NODE to string for storage in a sem database.

We store node id so that the node can be recovered and title as that's
the 'content' that's used for generating embedding."
  (let ((id (org-roam-node-id node))
        (title (org-roam-node-id node)))
    (prin1-to-string `((id . ,id)
                       (title . ,title)))))

(defun org-roam-sem-store-nodes (nodes)
  "Store all NODES in the database."
  (sem-add-batch-chunked org-roam-sem-db nodes #'org-roam-node-embed-batch 50
                         #'org-roam-node--sem-write-fn org-roam-sem-nodes-table t))

(defun org-roam-sem--link-context (link-data)
  "Get surrounding text context for the given LINK-DATA."
  (let ((source-node (org-roam-populate (org-roam-node-create :id (nth 0 link-data)))))
    (save-excursion
      (with-current-buffer (find-file-noselect (org-roam-node-file source-node))
        (goto-char (nth 2 link-data))
        (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))

(defun org-roam-sem--link-categories (link-data)
  "Return a list of categories that specify the link in LINK-DATA.

TODO: This could be designed better.  The main purpose is to tell what
type of link one is."
  (let ((source-node (org-roam-populate (org-roam-node-create :id (nth 0 link-data))))
        (dest-node (org-roam-populate (org-roam-node-create :id (nth 1 link-data)))))
    (list (org-roam-node-category source-node)
          (org-roam-node-category dest-node))))

(defun org-roam-links ()
  "Return a list of all id type links stored in the org-roam database.

This might be a heavy operation since reading link context involves
opening buffers where link is established."
  (let ((sql [:select [source dest pos properties] :from links :where (= type "id")]))
    (mapcar (lambda (link-data)
              (list :source-id (nth 0 link-data)
                    :dest-id (nth 1 link-data)
                    :context (org-roam-sem--link-context link-data)
                    :categories (org-roam-sem--link-categories link-data)))
            (org-roam-db-query sql))))

(defun org-roam-sem-link-embed-batch (links)
  "Embed a list of LINKS."
  (sem-embed-default links (lambda (link) (plist-get link :context))))

(defun org-roam-sem-store-links (links)
  "Save LINKS for sem search."
  (sem-add-batch-chunked org-roam-sem-db links #'org-roam-sem-link-embed-batch 50
                         #'prin1-to-string org-roam-sem-links-table t))

;;;###autoload
(defun org-roam-sem-sync ()
  "Sync all nodes and links with the vector storage."
  (interactive)
  (let ((nodes (org-roam-node-list))
        (links (org-roam-links)))
    (org-roam-sem-store-nodes nodes)
    (message "Upserted %d nodes" (length nodes))
    (org-roam-sem-store-links links)
    (message "Stored %d links" (length links))))

(defun org-roam-sem-similar (node)
  "Find nodes similar to NODE by matching with stored vectors.

TODO: The API used to take care of pruning items that are already linked
to the node.  This needs to be brought back.
TODO: Also add score filtering at 0.6."
  (mapcar (lambda (it) (cons (org-roam-populate (org-roam-node-create :id (alist-get 'id (cdr it))))
                             (car it)))
          (sem-similar org-roam-sem-db node 10 #'org-roam-node-embed #'read org-roam-sem-nodes-table)))

(defun org-roam-sem--insert-link-results (results)
  "Insert the search RESULTS from semantic link search.

Each item in results is a pair of score and link."
  ;; We group the results in link types (category to category mapping)
  (let ((groups (seq-group-by (lambda (result) (plist-get (cdr result) :categories)) results)))
    (dolist (group (seq-sort-by (lambda (group-pair) (caar (cdr group-pair))) #'> groups))
      (magit-insert-section (org-roam-link-category-section)
        (magit-insert-heading (format "%s (%s)" (string-join (car group) " ↔ ") (length (cdr group))))
        (dolist (result (cdr group))
          (let ((source-node (org-roam-populate (org-roam-node-create :id (plist-get (cdr result) :source-id)))))
            (magit-insert-section (org-roam-link-section)
              (magit-insert-heading (concat (format "[%.3f] " (car result))
                                            (propertize (org-roam-node-title source-node)
                                                        'font-lock-face 'org-roam-title)))
              (magit-insert-section section (org-roam-preview-section)
                                    (oset section file (org-roam-node-file source-node))
                                    (insert (org-roam-fontify-like-in-org-mode (plist-get (cdr result) :context)) "\n\n")))))))))

(defun org-roam-sem-search-links (term)
  "Search links using semantic search using the TERM."
  (interactive "sSearch: ")
  (if-let ((results (sem-similar org-roam-sem-db term 10 (lambda (text) (aref (sem-embed-default (list text)) 0))
                                 #'read org-roam-sem-links-table)))
      (progn
        (with-current-buffer (get-buffer-create "*org-roam-link-search*")
          (org-roam-mode)
          (visual-line-mode)
          (read-only-mode -1)
          (delete-region (point-min) (point-max))
          (org-roam-sem--insert-link-results results)
          (read-only-mode)
          (goto-char (point-min)))
        (switch-to-buffer "*org-roam-link-search*"))
    (message "No link results found for term: %s" term)))

(provide 'org-roam-sem)

;;; org-roam-sem.el ends here
