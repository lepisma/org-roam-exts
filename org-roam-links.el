;;; org-roam-links.el --- Link related extensions for Org-Roam -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Link related extensions for Org-Roam
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
(require 'popwin)
(require 'org-roam-category)
(require 'org-roam-sem)

(add-to-list 'display-buffer-alist
             '("\\*org-roam\\*"
               (display-buffer-in-direction)
               (direction . right)
               (window-width . 0.33)
               (window-height . fit-window-to-buffer)))

(defun org-roam-preview-line-function ()
  "Org roam preview function that only returns the current line.
This helps in showing more links in the roam buffer."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(setq org-roam-preview-function #'org-roam-preview-line-function)

(cl-defun org-roam-node-insert-similarlink-section (&key dest-node point properties)
  "Show section for a similar node, DEST-NODE.
For details of other arguments, check `org-roam-node-insert-backlink-section'."
  (magit-insert-section section (org-roam-node-section)
    (let ((outline (if-let ((outline (plist-get properties :outline)))
                       (mapconcat #'org-link-display-format outline " > ")
                     "Top")))
      (insert (concat (format " ~ [%.3f] " (plist-get properties :score))
                      (propertize (org-roam-node-title dest-node)
                                  'font-lock-face 'org-roam-title))))
    (magit-insert-heading)
    (oset section node dest-node)
    (magit-insert-section section (org-roam-preview-section)
      (insert "\n")
      (oset section file (org-roam-node-file dest-node))
      (oset section point point)
      (insert "\n"))))

(cl-defun org-roam-node-insert-frontlink-section (&key dest-node point properties)
  "Show section for a link from current node to the DEST-NODE.
For details of other arguments, check `org-roam-node-insert-backlink-section'."
  (magit-insert-section section (org-roam-node-section)
    (let ((outline (if-let ((outline (plist-get properties :outline)))
                       (mapconcat #'org-link-display-format outline " > ")
                     "Top")))
      (insert (concat " → TO "
                      (propertize (org-roam-node-title dest-node)
                                  'font-lock-face 'org-roam-title))))
    (magit-insert-heading)
    (oset section node dest-node)
    (magit-insert-section section (org-roam-preview-section)
      (insert (org-roam-fontify-like-in-org-mode
               (org-roam-preview-get-contents (org-roam-node-file org-roam-buffer-current-node) point))
              "\n")
      (oset section file (org-roam-node-file dest-node))
      (oset section point point)
      (insert ?\n))))

(cl-defun org-roam-node-insert-backlink-section (&key source-node point properties)
  "Insert section for a link from SOURCE-NODE to some other node.
The other node is normally `org-roam-buffer-current-node'.

SOURCE-NODE is an `org-roam-node' that links or references with
the other node.

POINT is a character position where the link is located in
SOURCE-NODE's file.

PROPERTIES (a plist) contains additional information about the
link.

Despite the name, this function actually inserts 2 sections at
the same time:

1. `org-roam-node-section' for a heading that describes
   SOURCE-NODE. Acts as a parent section of the following one.

2. `org-roam-preview-section' for a preview content that comes
   from SOURCE-NODE's file for the link (that references the
   other node) at POINT. Acts a child section of the previous
   one."
  (magit-insert-section section (org-roam-node-section)
    (let ((outline (if-let ((outline (plist-get properties :outline)))
                       (mapconcat #'org-link-display-format outline " > ")
                     "Top")))
      (insert (concat " ← FROM "
                      (propertize (org-roam-node-title source-node)
                                  'font-lock-face 'org-roam-title)
                      (format " (%s)"
                              (propertize outline 'font-lock-face 'org-roam-olp)))))
    (magit-insert-heading)
    (oset section node source-node)
    (magit-insert-section section (org-roam-preview-section)
      (insert (org-roam-fontify-like-in-org-mode
               (org-roam-preview-get-contents (org-roam-node-file source-node) point))
              "\n")
      (oset section file (org-roam-node-file source-node))
      (oset section point point)
      (insert ?\n))))

(defun org-roam-frontlinks-get (node)
  "Return the frontlinks for NODE in the same structure like backlink."
  (let* ((sql [:select [source dest pos properties]
                       :from links
                       :where (= source $s1)
                       :and (= type "id")])
         (frontlinks (org-roam-db-query sql (org-roam-node-id node))))
    (cl-loop for frontlink in frontlinks
             collect (pcase-let ((`(,source-id ,dest-id ,pos ,properties) frontlink))
                       (org-roam-populate
                        (org-roam-backlink-create
                         :source-node (org-roam-node-create :id source-id)
                         :target-node (org-roam-node-create :id dest-id)
                         :point pos
                         :properties properties))))))

(defun org-roam--frontlink-p (backlink)
  "Tell if BACKLINK is actually a frontlink.

This is done by checking for match with
`org-roam-buffer-current-node' so should only be used where this
logic makes sense."
  (equal (org-roam-backlink-source-node backlink) org-roam-buffer-current-node))

(defun org-roam-links-section (node)
  "Rich links from and to the node presented in categories."
  (when-let ((links (seq-sort #'org-roam-backlinks-sort (append (org-roam-backlinks-get node :unique nil)
                                                                (org-roam-frontlinks-get node)))))
    (let ((groups (seq-group-by (lambda (link)
                                  (org-roam-node-category (if (org-roam--frontlink-p link)
                                                              (org-roam-backlink-target-node link)
                                                            (org-roam-backlink-source-node link))))
                                links)))
      (dolist (group (seq-sort-by (lambda (group-pair)
                                    (length (member (car group-pair) (list "Unread Bookmarks"
                                                                           "Person"
                                                                           "Research Works"
                                                                           "Projects or Products"
                                                                           "Recipes"
                                                                           "Journal Entries"
                                                                           "Read Bookmarks"
                                                                           "Other Links"))))
                                  #'>
                                  groups))
        (magit-insert-section (org-roam-backlinks)
          (magit-insert-heading (format "%s (%s)" (car group) (length (cdr group))))
          (dolist (link (cdr group))
            (if (org-roam--frontlink-p link)
                (org-roam-node-insert-frontlink-section
                 :dest-node (org-roam-backlink-target-node link)
                 :point (org-roam-backlink-point link)
                 :properties (org-roam-backlink-properties link))
              (org-roam-node-insert-backlink-section
               :source-node (org-roam-backlink-source-node link)
               :point (org-roam-backlink-point link)
               :properties (org-roam-backlink-properties link))))
          (insert ?\n))))))

(defun org-roam-similar-section (node)
  "Show section where nodes similar to current are displayed."
  (when-let ((similar-nodes-with-scores (org-roam-sem-similar node)))
    (magit-insert-section (org-roam-similar)
      (magit-insert-heading (format "Similar Nodes (%s)" (length similar-nodes-with-scores)))
      (dolist (snode-score similar-nodes-with-scores)
        (org-roam-node-insert-similarlink-section
         :dest-node (car snode-score)
         :point 1
         :properties (list :score (cdr snode-score)))))))

(setq org-roam-mode-sections (list #'org-roam-links-section
                                   #'org-roam-reflinks-section
                                   #'org-roam-similar-section))

(cl-defmethod org-roam-node-connections-from ((node org-roam-node))
  "Return all direct nodes going out `from' the given NODE."
  (let ((result (org-roam-db-query [:select [dest] :from links :where (= source $s1) :and (= type "id")] (org-roam-node-id node))))
    (mapcar (lambda (it) (org-roam-populate (org-roam-node-create :id (car it)))) result)))

(cl-defmethod org-roam-node-connections-to ((node org-roam-node))
  "Return all direct nodes going in `to' the given NODE."
  (let ((result (org-roam-db-query [:select [source] :from links :where (= dest $s1) :and (= type "id")] (org-roam-node-id node))))
    (mapcar (lambda (it) (org-roam-populate (org-roam-node-create :id (car it)))) result)))

(cl-defmethod org-roam-node-connections ((node org-roam-node))
  "Return all unique connected nodes to the given NODE."
  (seq-uniq (append (org-roam-node-connections-from node)
                    (org-roam-node-connections-to node))))

(defun org-roam--advance-path (path)
  (let ((nodes (org-roam-node-connections (car (last path)))))
    (mapcar (lambda (node) (append path (list node))) (seq-remove (lambda (node) (member node path)) nodes))))

(defun org-roam--advance-paths (paths)
  "Advance PATHS (list of nodes) one step ahead for search."
  (seq-uniq (apply #'append (mapcar #'org-roam--advance-path paths))))

(defun org-roam-trace-path (source-node dest-node &optional current-paths)
  "Trace shortest path from SOURCE-NODE to DEST-NODE and return full
trace of nodes in a list. Return nil if no connection found.

We just do a simple one-sided BFS here. CURRENT-PATHS keep the
active search fringe."

  (let ((current-paths (or current-paths (list (list source-node)))))
    (if-let ((final-path (seq-some (lambda (path) (when (equal (car (last path)) dest-node) path)) current-paths)))
        ;; If any of the fringe paths have reached destination, stop and return the
        ;; first path. This will still get us the shortest path.
        final-path
      (org-roam-trace-path source-node dest-node (org-roam--advance-paths current-paths)))))

(defun org-roam-display-trace (nodes)
  "Show the trace of NODES in a buffer with clickable links."
  (with-current-buffer (get-buffer-create "*org-roam-trace*")
    (read-only-mode -1)
    (delete-region (point-min) (point-max))
    (insert "Path trace\n\n")
    (dolist (node nodes)
      (insert "| ")
      (insert-text-button (format "%s" (org-roam-node-title node)) 'follow-link t 'action `(lambda (_) (org-id-goto ,(org-roam-node-id node))))
      (insert "\n"))
    (read-only-mode)
    (popwin:popup-buffer "*org-roam-trace*")))

(defun org-roam-relevance ()
  "Show how the current node is connected to my plans."
  (interactive)
  (let ((plans-node (org-roam-populate (org-roam-node-create :id "a3278b4f-bfa5-4cbc-8b78-ddf92394d606"))))
    (if-let ((path (org-roam-trace-path plans-node (org-roam-node-at-point))))
        (org-roam-display-trace path)
      (message "No connection to plans found."))))

(provide 'org-roam-links)

;;; org-roam-links.el ends here
