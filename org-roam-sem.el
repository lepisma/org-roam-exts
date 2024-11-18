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


(require 'url)
(require 'json)
(require 'org-roam)
(require 'org-roam-category)

(defun org-roam-sem--request (path)
  "Send GET request to sem server with given PATH and return json
response."
  (let* ((url (format "http://localhost:8431%s" path))
         (buffer (url-retrieve-synchronously url)))
    (when buffer
      (with-current-buffer buffer
        (goto-char (point-min))
        (re-search-forward "\n\n")
        (let ((json-array-type 'list))
          (json-read))))))

(defun org-roam-sem--request-search-links (term)
  (org-roam-sem--request (format "/search/links/%s" (url-encode-url term))))

(defun org-roam-sem--request-similar (id)
  "Send request to sem server with node ID and return a list of ID
and score pairs for similar nodes."
  (org-roam-sem--request (format "/similar/%s" id)))

(defun org-roam-sem-similar (node)
  "Find nodes similar to NODE by using the sem API.

The API takes care of pruning items that are already linked to the node."
  (mapcar (lambda (id-score) (cons (org-roam-populate (org-roam-node-create :id (car id-score)))
                                   (cadr id-score)))
          (org-roam-sem--request-similar (org-roam-node-id node))))

(defun org-roam-sem--link-context (link-data)
  (let ((source-node (org-roam-populate (org-roam-node-create :id (nth 0 link-data)))))
    (save-excursion
      (with-current-buffer (find-file-noselect (org-roam-node-file source-node))
        (goto-char (nth 2 link-data))
        (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))

(defun org-roam-sem--link-categories (link-data)
  (let ((source-node (org-roam-populate (org-roam-node-create :id (nth 0 link-data))))
        (dest-node (org-roam-populate (org-roam-node-create :id (nth 1 link-data)))))
    (list (org-roam-node-category source-node)
          (org-roam-node-category dest-node))))

(defun org-roam-sem-prep-links (output-filepath)
  "Collect links and save metadata for OUTPUT-FILEPATH for sem search."
  (let* ((sql [:select [source dest pos properties] :from links :where (= type "id")])
         (output (mapcar (lambda (link-data)
                           (list :source-id (nth 0 link-data)
                                 :dest-id (nth 1 link-data)
                                 :context (org-roam-sem--link-context link-data)
                                 :categories (org-roam-sem--link-categories link-data)))
                         (org-roam-db-query sql))))
    (with-temp-file output-filepath
      (insert (json-encode (list :links (vconcat output)))))))

(defun org-roam-sem--insert-link-results (results)
  "Insert the search RESULTS from semantic link search."
  ;; We group the results in link types (category to category mapping)
  (let ((groups (seq-group-by (lambda (result) (alist-get 'categories (car result))) results)))
    (dolist (group (seq-sort-by (lambda (group-pair) (cadar (cdr group-pair))) #'> groups))
      (magit-insert-section (org-roam-link-category-section)
        (magit-insert-heading (format "%s (%s)" (string-join (car group) " ↔ ") (length (cdr group))))
        (dolist (result (cdr group))
          (let ((source-node (org-roam-populate (org-roam-node-create :id (alist-get 'source-id (car result))))))
            (magit-insert-section (org-roam-link-section)
              (magit-insert-heading (concat (format "[%.3f] " (cadr result))
                                            (propertize (org-roam-node-title source-node)
                                                        'font-lock-face 'org-roam-title)))
              (magit-insert-section section (org-roam-preview-section)
                                    (oset section file (org-roam-node-file source-node))
                                    (insert (org-roam-fontify-like-in-org-mode (alist-get 'context (car result))) "\n\n")))))))))

(defun org-roam-sem-search-links (term)
  "Search links using semantic search using the TERM."
  (interactive "sSearch: ")
  (if-let ((results (org-roam-sem--request-search-links term)))
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
