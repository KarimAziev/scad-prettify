;;; scad-prettify.el --- Prettify scad code -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/scad-prettify
;; Version: 0.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "29.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Prettify OpenSCAD code

;;; Code:




(defvar scad-prettify--variable-regex
  "^[[:space:]]*\\([A-Za-z_][A-Za-z0-9_]*\\)[[:space:]]*\\(=\\)[[:space:]]*\\([^;]+;\\)")


(defun scad-prettify--format-braces ()
  "Remove extra blank lines before closing braces in the buffer."
  (save-excursion
    (save-match-data
      (while (re-search-forward
              "\\([\n][[:space:]]*[\n]\\)\\([[:space:]]*[\n][[:space:]]*\\)}"
              nil t
              1)
        (save-excursion
          (let ((start)
                (end (line-beginning-position)))
            (while (and (zerop (forward-line -1))
                        (looking-at "\\(^[[:space:]]*[\n][[:space:]]*\\)"))
              (setq start (point)))
            (when (and start end)
              (delete-region start
                             end))))))))

(defun scad-prettify--format-variables ()
  "Align variable assignments by padding spaces for consistent formatting."
  (let ((longest-len)
        (last-var-end))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while
            (when (and (re-search-forward scad-prettify--variable-regex nil t 1)
                       (zerop (car (syntax-ppss (point)))))
              (let ((var-name (match-string-no-properties 1)))
                (setq last-var-end (match-end 0))
                (setq longest-len (max (or longest-len 0)
                                       (length var-name))))))
        (when (and last-var-end longest-len)
          (goto-char last-var-end)
          (while
              (re-search-backward scad-prettify--variable-regex nil t 1)
            (let ((var-name (match-string-no-properties 1))
                  (value (match-string-no-properties 3))
                  (start (match-beginning 0))
                  (end (match-end 0)))
              (let ((rep (concat var-name
                                 (make-string
                                  (1+ (- longest-len
                                         (length var-name)))
                                  ?\s)
                                 "= ")))
                (if (not (string-match-p "\n" value))
                    (setq rep (concat rep value))
                  (let* ((lines (split-string value "\n" t))
                         (first-line (pop lines))
                         (spaces (make-string (1+ (length rep)) ?\s)))
                    (setq lines (mapconcat (lambda (line)
                                             (concat spaces
                                                     (string-trim line)))
                                           lines
                                           "\n"))
                    (setq rep (concat rep first-line "\n" lines))))
                (delete-region start end)
                (save-excursion
                  (insert rep))))))))))



;;;###autoload
(defun scad-prettify-buffer ()
  "Format the buffer by adjusting braces, aligning variables, and indenting."
  (interactive)
  (with-undo-amalgamate
    (scad-prettify--format-braces)
    (scad-prettify--format-variables)
    (indent-region (point-min)
                   (point-max))))

;;;###autoload
(define-minor-mode scad-prettify-mode
  "Enable automatic formatting of SCAD code before saving the buffer."
  :lighter " scad-prettify"
  :global nil
  (if scad-prettify-mode
      (add-hook 'before-save-hook #'scad-prettify-buffer nil 'local)
    (remove-hook 'before-save-hook #'scad-prettify-buffer 'local)))

(provide 'scad-prettify)
;;; scad-prettify.el ends here