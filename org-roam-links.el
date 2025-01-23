;;; org-roam-links.el --- Link and graph related functions for Org-Roam -*- lexical-binding: t; -*-

;; Copyright (c) 2024-2025 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Link and graph related functions for Org-Roam
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

(provide 'org-roam-links)

;;; org-roam-links.el ends here
