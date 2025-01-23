;;; org-roam-exts-utils.el --- Utilities -*- lexical-binding: t; -*-

;; Copyright (c) 2025 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>

;;; Commentary:

;; Utilities
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

(provide 'org-roam-exts-utils)

;;; org-roam-exts-utils.el ends here
