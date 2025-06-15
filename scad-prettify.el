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

;; This is a very simple minor mode to format OpenSCAD code. Mostly it:
;; - aligns variable assignments
;; - removes multiple new lines before braces
;; - applies some regexp transformations
;; - reindent buffer

;; Usage

;; M-x `scad-prettify-mode':   to toggle minor mode in the current buffer that
;;                             formats the whole buffer before saving
;; M-x `scad-prettify-buffer': to directly format buffer.

;; Customization:

;; `scad-prettify-formatters'              : to add or remove specific formatters.
;; `scad-prettify-regexp-replacement-rules': customize rules for regular expressions based formatting.


;;; Code:

(require 'subr-x)
(eval-when-compile
  (require 'rx))
(require 'scad-mode)


(defcustom scad-prettify-regexp-replacement-rules `((re-search-forward
                                                     ,(rx (seq "\n"
                                                           (group
                                                            (one-or-more " "))
                                                           "\n"))
                                                     ""
                                                     1)
                                                    (re-search-forward
                                                     ,(rx (group bol
                                                           (= 2 "\n")))
                                                     "\n" 1)
                                                    (re-search-forward
                                                     ,(rx (seq (any "([")
                                                           (group
                                                            (one-or-more
                                                             (any "\t\n ")))))
                                                     "" 1)
                                                    (re-search-forward
                                                     ,(rx
                                                       (seq
                                                        (group
                                                         (one-or-more
                                                          (any
                                                           "\t\n ")))
                                                        (group
                                                         (or ")" "]"))))
                                                     "" 1)
                                                    (re-search-forward
                                                     ,(rx
                                                       (seq
                                                        (group
                                                         (any "A-Za-z" "_")
                                                         (zero-or-more
                                                          (any "0-9A-Za-z" "_")))
                                                        (group
                                                         (one-or-more
                                                          (any "\t\n ")))
                                                        "("))
                                                     "" 2
                                                     ((:subexp-start 1
                                                       :not :symbol-at-point
                                                       (for if let))))
                                                    (re-search-forward
                                                     ,(rx
                                                       (seq symbol-start
                                                        (group
                                                         (or "for" "let" "if"))
                                                        symbol-end
                                                        (group "(")))
                                                     " (" 2)
                                                    (re-search-forward
                                                     ,(rx (seq ")"
                                                           (group space
                                                            (one-or-more space))))
                                                     "" 1)
                                                    (re-search-forward
                                                     "){"
                                                     ") {" 0))
  "Alist of regular expression replacement rules for prettifying SCAD code.

Each element in this list defines a rule for matching and replacing text and
should be a list of the form: (SEARCH-FN REGEXP REPLACEMENT SUBEXP [MATCHERS])

- SEARCH-FN: A function (either `re-search-forward' or `re-search-backward')
  that scans the buffer in the specified direction to find matches for REGEXP.

- REGEXP: The regular expression to match text patterns in the buffer.

- REPLACEMENT: The string that will replace the matched text (or just the
  specified subexpression).

- SUBEXP: An integer indicating the subexpression index to replace (0 means the
  entire match).

- MATCHERS (optional): A list of extra matching criteria that are further
  applied to a found match.
  Each extra matcher specifies:
    - A position specifier (`:subexp-start' or `:subexp-end') indicating whether
      the condition should be evaluated at the beginning or end of the specified
      subexpression.
    - One or more conditions that must hold at that position.
      These conditions can be of different types:
        - `:looking-at': Checks whether the text at that position matches a
          given regular expression.
        - `:symbol-at-point': Verifies if the symbol at that position is one of
          a given set.
        - `:predicate': Calls a custom function to determine whether the
          condition is met.
    - An optional `:not' modifier that inverts the result of the condition.

When a regexp match is found, any extra matchers are evaluated immediately after
the match (but before performing the replacement).

For the replacement to occur, every extra matcher must succeed (i.e. return
non-nil), if any matcher returns nil (or, when using `:not', returns non-nil
when negated), then that candidate match is rejected and skipped."
  :group 'scad-prettify
  :type '(repeat
          (list
           (radio
            :tag "If research"
            :value re-search-forward
            (const :tag "forward" re-search-forward)
            (const :tag "backward" re-search-backward))
           (regexp :tag "regular expression")
           (string
            :tag "then replace with"
            :value "")
           (integer :tag "subexpression" 0)
           (repeat :tag "Extra matchers"
            (list
             :format "%v"
             (list
              :inline t
              :format "%t\n%v"
              :tag "If at"
              (radio
               :format "%v"
               :value
               :subexp-start
               (const
                :tag "start"
                :value :subexp-start)
               (const
                :tag "end"
                :value :subexp-end))
              (integer :tag "of subexpression"))
             (list
              :format "%v"
              :inline t
              (set
               :inline t
               :tag "not"
               :format "%t %v"
               (const
                :format ""
                :not))
              (radio
               :inline t
               :format "%v"
               :value :looking-at
               (list
                :inline t
                :tag "looking at"
                (const
                 :format ""
                 :value :looking-at)
                (regexp :tag
                 "regular expression"))
               (list
                :format "%v"
                :inline t
                (const
                 :format " "
                 :value :symbol-at-point)
                (repeat :tag "symbol at point one of" (symbol)))
               (list
                :tag "predicate"
                :inline t
                (const
                 :format ""
                 :value :predicate)
                (function :tag "Custom predicate function")))))))))

(defcustom scad-prettify-formatters '(scad-prettify-format-braces
                                      scad-prettify-align-variables)
  "List of functions to format SCAD code, such as adjusting braces and alignment.

Each function in the list should take no arguments and perform a specific
formatting task.

The functions are executed in the order they appear in the list when formatting
is triggered."
  :group 'scad-prettify
  :type 'hook)

(defconst scad-prettify--variable-regex
  (rx (seq bol
           (zero-or-more space)
           (group
            (any "A-Za-z" "_")
            (zero-or-more
             (any "0-9A-Za-z" "_")))
           (zero-or-more space)
           (group "=")
           (zero-or-more space)
           (group
            (one-or-more
             (not (any ";")))
            ";")))
  "Regex pattern matching SCAD variable declarations.")


(defun scad-prettify--inside-string-or-comment-p ()
  "Determine if the point is inside a string or comment."
  (let ((pps (syntax-ppss (point))))
    (or (nth 3 pps)
        (nth 4 pps))))

(defun scad-prettify--format-by-regex ()
  "Iterate over regex rules to replace text patterns in a buffer."
  (pcase-dolist (`(,search_sym ,regexp ,replacement ,subexp ,matchers)
                 scad-prettify-regexp-replacement-rules)
    (scad-prettify--replace-in-buffer search_sym regexp replacement subexp
                                      matchers)))

(defun scad-prettify--replace-in-buffer (search_sym regexp replacement &optional
                                                    subexp matchers)
  "Replace occurrences of REGEXP with REPLACEMENT, skipping strings/comments.

Argument SEARCH_SYM is the function used to search for matches, such as
`re-search-forward'.

Argument REGEXP is the regular expression pattern to search for in the buffer.

Argument REPLACEMENT is the string to replace matches of the regular expression.

Optional argument SUBEXP specifies which subexpression of the match to replace.

Optional argument MATCHERS is a list of plists used to further filter matches
based on additional criteria."
  (save-excursion
    (save-match-data
      (goto-char (if (eq search_sym 're-search-forward)
                     (point-min)
                   (point-max)))
      (while (funcall search_sym regexp nil t 1)
        (unless (scad-prettify--inside-string-or-comment-p)
          (when (or (not matchers)
                    (not (catch 'mismatch
                           (dolist (matchers-plist matchers)
                             (let ((pos (or (memq :subexp-start matchers-plist)
                                            (memq :subexp-end matchers-plist)))
                                   (pl matchers-plist)
                                   (negatep (memq :not matchers-plist)))
                               (save-excursion
                                 (goto-char (funcall (if (eq (car pos)
                                                             :subexp-start)
                                                         #'match-beginning
                                                       #'match-end)
                                                     (cadr pos)))
                                 (save-match-data
                                   (while pl
                                     (let ((key (car pl))
                                           (matcher (cadr pl)))
                                       (cond ((eq key :not)
                                              (setq pl (cdr pl)))
                                             ((memq key '(:subexp-end
                                                          :subexp-start))
                                              (setq pl (cddr pl)))
                                             (t
                                              (let ((result
                                                     (pcase key
                                                       (:looking-at
                                                        (looking-at matcher 0))
                                                       (:symbol-at-point
                                                        (let
                                                            ((sym
                                                              (symbol-at-point)))
                                                          (memq sym matcher)))
                                                       (_
                                                        (save-excursion
                                                          (funcall matcher))))))
                                                (when negatep
                                                  (setq result (not result)))
                                                (unless result
                                                  (throw 'mismatch t))
                                                (setq pl (cddr pl))))))))))))))
            (replace-match replacement nil nil nil subexp)))))))


(defun scad-prettify-format-braces ()
  "Remove extra blank lines before closing braces in the buffer."
  (save-excursion
    (goto-char (point-min))
    (save-match-data
      (while (re-search-forward
              "\\([\n][[:space:]]*[\n]\\)\\([[:space:]]*\\)}"
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

(defun scad-prettify-align-variables ()
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
  (let* ((buff (current-buffer))
         (result (with-temp-buffer
                   (insert-buffer-substring buff)
                   (let ((scad-mode-hook nil))
                     (scad-mode))
                   (let ((indent-tabs-mode nil)
                         (inhibit-message t))
                     (scad-prettify--format-by-regex)
                     (run-hooks 'scad-prettify-formatters)
                     (indent-region (point-min)
                                    (point-max))
                     (buffer-string)))))
    (unless (string= (buffer-substring-no-properties (point-min)
                                                     (point-max))
                     result)
      (replace-region-contents (point-min)
                               (point-max)
                               (lambda () result)))))

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