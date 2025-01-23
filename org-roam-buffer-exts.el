;;; org-roam-buffer-exts.el --- Roam buffer extensions -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Roam buffer extensions
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

(require 'cl-macs)
(require 'eieio)
(require 'magit-section)
(require 'org-roam-category)
(require 'org-roam-sem)

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
  "Show section where nodes similar to NODE are displayed."
  (when-let ((similar-nodes-with-scores (org-roam-sem-similar node)))
    (magit-insert-section (org-roam-similar)
      (magit-insert-heading (format "Similar Nodes (%s)" (length similar-nodes-with-scores)))
      (dolist (snode-score similar-nodes-with-scores)
        (org-roam-node-insert-similarlink-section
         :dest-node (car snode-score)
         :point 1
         :properties (list :score (cdr snode-score)))))))

;;;###autoload
(defun org-roam-buffer-exts-enable ()
  "Enable extensions for Org Roam buffer."
  (interactive)
  (setq org-roam-preview-function #'current-line-string
        org-roam-mode-sections (list #'org-roam-links-section
                                     #'org-roam-reflinks-section
                                     #'org-roam-similar-section))
  (org-roam-sem-setup))

(provide 'org-roam-buffer-exts)

;;; org-roam-buffer-exts.el ends here
