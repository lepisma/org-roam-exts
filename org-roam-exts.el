;;; org-roam-exts.el --- Org Roam link extensions -*- lexical-binding: t; -*-

;; Copyright (c) 2024 Abhinav Tushar

;; Author: Abhinav Tushar <abhinav@lepisma.xyz>
;; Version: 0.0.1
;; Package-Requires: ((emacs "29") (org-roam "2.2.2") (org "9.7.15") (popwin "1.0.2"))
;; Keywords: roam, org-mode, writing
;; URL: https://github.com/lepisma/org-roam-exts

;;; Commentary:

;; Org Roam link extensions
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

(require 'org-roam-category)
(require 'org-roam-sidekick)
(require 'org-roam-sem)
(require 'org-roam-links)

(defun org-roam-exts-show-buffer ()
  "Show org-roam buffer if it's not visible."
  (pcase (org-roam-buffer--visibility)
    ((or 'exists 'none)
     (progn
       (display-buffer (get-buffer-create org-roam-buffer))
       (org-roam-buffer-persistent-redisplay)))))

;;;###autoload
(defun org-roam-exts-enable ()
  "Enable org-roam-exts."
  (interactive)
  (setq org-roam-preview-function #'org-roam-preview-line-function
        org-roam-mode-sections (list #'org-roam-links-section
                                     #'org-roam-reflinks-section
                                     #'org-roam-similar-section))
  (org-roam-sem-setup)
  (add-hook 'org-roam-find-file-hook #'org-roam-exts-show-buffer))

(provide 'org-roam-exts)

;;; org-roam-exts.el ends here
