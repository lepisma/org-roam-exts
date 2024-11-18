;;; org-roam-sidekick.el --- Sidekick for exposing Org-Roam to external programs -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Sidekick for exposing Org-Roam to external programs
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
(require 'org-protocol)

(defun org-roam-sk-find-ref (url)
  "Return a node if the given URL is saved in the database.

TODO: This needs url normalization since there are extra slashes
      and what not. We will handle this problem later as needed."
  (when-let ((id (caar (org-roam-db-query [:select [node_id] :from refs :where (= ref $s1)] (string-join (cdr (string-split url ":")) "")))))
    (org-roam-populate (org-roam-node-create :id id))))

(defun org-roam-sk-kagi-search-term (url)
  "Extract the search term from a Kagi search URL."
  (let ((query-start (string-match "\\?q=" url)))
    (when query-start
      (let ((query-string (substring url (+ query-start 3))))
        (if (string-match "&" query-string)
            (substring query-string 0 (match-beginning 0))
          (string-replace "+" " " (url-unhex-string query-string)))))))

(defun current-line-string ()
  "Return current line string."
  (buffer-substring-no-properties (line-beginning-position) (line-end-position)))

(defun org-file-title (filepath)
  "Return title for the org file at given FILEPATH."
  (save-excursion
    (with-temp-buffer
      (setq-local case-fold-search t)
      (insert-file-contents filepath nil nil 1000)
      (goto-char (point-min))
      ;; We assume every file has title
      (when (re-search-forward "^#\\+TITLE:\\(.*\\)$")
        (string-trim (match-string-no-properties 1))))))

(defun org-roam-sk--recoll-parse (text)
  "Parse the complete TEXT coming from recollq search and return a
list of search results as plists."
  (with-temp-buffer
    (let ((entries)
          (current-line))
      (insert text)
      (goto-line 4)  ; Skipping the search metadata
      (setq current-line (string-trim (current-line-string)))
      (while (not (string= "" current-line))
        (let* ((filepath (string-remove-suffix "]" (string-remove-prefix "[file://" (nth 1 (string-split current-line "\t"))))))
          (re-search-forward "ABSTRACT" nil t nil)
          (push-mark)
          (re-search-forward "/ABSTRACT" nil t nil)
          (push (list :filepath filepath
                      :heading-trace "NA"
                      :title (org-file-title filepath)
                      :abstract (string-trim (buffer-substring-no-properties (mark) (- (point) (length "/ABSTRACT")))))
                entries))
        (pop-mark)
        (next-line)
        (setq current-line (string-trim (current-line-string)))
        (push-mark))
      entries)))

(defun org-roam-sk--recoll-parse-and-insert (text)
  "Parse the complete recoll result in TEXT and insert in current
buffer."
  (let ((entries (org-roam-sk--recoll-parse text)))
    (magit-insert-section (org-roam-sk-recoll-section)
      (magit-insert-heading (format "Search Results (%s)" (length entries)))
      (dolist (entry entries)
        (magit-insert-section (org-roam-sk-recoll-section)
          (magit-insert-heading (propertize (plist-get entry :title)
                                            'font-lock-face 'org-roam-title))
          (magit-insert-section section (org-roam-preview-section)
            (oset section file (plist-get entry :filepath))
            (insert (plist-get entry :abstract) "\n\n")))))))

(defun org-roam-sk--recoll-filter (process string)
  "Filter function to accumulate the output of recoll search."
  (let ((current (or (process-get process 'accumulated-output) "")))
    (process-put process 'accumulated-output (concat current string))))

(defun org-roam-sk--recoll-sentinel (process event)
  "Recoll sentinel to do cleanups and talk to the process."
  (when (string= event "finished\n")
    (with-current-buffer (get-buffer-create "*org-roam-sk-search*")
      (org-roam-mode)
      (visual-line-mode)
      (read-only-mode -1)
      (delete-region (point-min) (point-max))
      (org-roam-sk--recoll-parse-and-insert (process-get process 'accumulated-output))
      (read-only-mode)
      (goto-char (point-min)))
    (switch-to-buffer "*org-roam-sk-search*")))

(defun org-roam-sk-recoll-search (term)
  "Perform a search using recoll."
  (interactive "sSearch: ")
  (make-process :name "recoll"
                :buffer "*org-roam-sk-search*"
                :command (list "recollq" "-A" term)
                :filter #'org-roam-sk--recoll-filter
                :sentinel #'org-roam-sk--recoll-sentinel))

(defun org-roam-sk-url-dispatch (url)
  "Take action on the URL based on various rules.
Most of these result in opening a buffer with something to
display in the sidekick way."
  (cond
   ((string-match "kagi.com/search" url)
    (org-roam-sk-recoll-search (org-roam-sk-kagi-search-term url)))
   ((org-roam-sk-find-ref url)
    ;; TODO: Fix multiple calls to find ref
    (find-file (org-roam-node-file (org-roam-sk-find-ref url)))
    (pcase (org-roam-buffer--visibility)
      ('visible nil)
      ((or 'exists 'none)
       (progn
         (display-buffer (get-buffer-create org-roam-buffer))
         (org-roam-buffer-persistent-redisplay)))))
   (t (message "Can't do anything for %s" url))))

(defun org-roam-sk-protocol-open (info)
  (raise-frame)
  (org-roam-sk-url-dispatch (plist-get info :url))
  nil)

(push (list "org-roam-sk" :protocol "roam-sk" :function #'org-roam-sk-protocol-open)
      org-protocol-protocol-alist)

(provide 'org-roam-sidekick)

;;; org-roam-sidekick.el ends here
