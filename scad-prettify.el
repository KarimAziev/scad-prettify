;;; scad-prettify.el --- Prettify scad code -*- lexical-binding: t; -*-

;; Copyright (C) 2025 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/scad-prettify
;; Version: 0.1.0
;; Keywords: languages
;; Package-Requires: ((emacs "29.1") (scad-mode "96.0"))
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
(require 'project)


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
                                                     ,(rx
                                                       (seq (any "([")
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
                                                     "" 1
                                                     ((:subexp-start 0
                                                                     :not
                                                                     :inside-comment-or-string)))
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
                                                                     :not
                                                                     :symbol-at-point
                                                                     (for if let))))
                                                    (re-search-forward
                                                     "[)]\\([\n]+[\s\t]*\\){"
                                                     "" 1
                                                     ((:subexp-start 0
                                                                     :not
                                                                     :inside-comment-or-string)))
                                                    (re-search-forward
                                                     ,(rx
                                                       (seq symbol-start
                                                            (group
                                                             (or "for" "let"
                                                                 "if"))
                                                            symbol-end
                                                            (group "(")))
                                                     " (" 2)
                                                    (re-search-forward
                                                     ,(rx
                                                       (seq ")"
                                                            (group space
                                                                   (one-or-more
                                                                    space))))
                                                     "" 1)
                                                    (re-search-forward
                                                     "){"
                                                     ") {" 0)
                                                    (re-search-forward
                                                     "^\\([\n][\n]+\\)"
                                                     "\n" 0)
                                                    (re-search-backward
                                                     "\\([+]\\)[0-9a-z]+"
                                                     " \\& "
                                                     1
                                                     ((:subexp-end 1
                                                                   :not
                                                                   :inside-comment-or-string)))
                                                    (re-search-backward
                                                     "\\([,]\\)[0-9a-z]+"
                                                     "\\& "
                                                     1
                                                     ((:subexp-end 1
                                                                   :not
                                                                   :inside-comment-or-string))))
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
                      :format "%t %v\n"
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
                        :format ""
                        :value :symbol-at-point)
                       (repeat :tag "symbol at point one of" (symbol)))
                      (list
                       :inline t
                       :tag "inside comment or string"
                       :format "%t%v\n"
                       (const
                        :format ""
                        :value :inside-comment-or-string))
                      (list
                       :inline t
                       :tag "inside comment"
                       :format "%t%v\n"
                       (const
                        :format ""
                        :value :inside-comment))
                      (list
                       :inline t
                       :tag "inside string"
                       :format "%t%v\n"
                       (const
                        :format ""
                        :value :inside-string))
                      (list
                       :tag "predicate"
                       :inline t
                       (const
                        :format ""
                        :value :predicate)
                       (function :tag "Custom predicate function")))))))))


(defcustom scad-prettify-formatters '(scad-prettify-format-braces
                                      scad-prettify-align-args
                                      scad-prettify-indent-buffer-ignore-comments
                                      scad-prettify-align-variables
                                      scad-prettify-sort-imports)
  "List of functions to format SCAD code, such as adjusting braces and alignment.

Each function in the list should take no arguments and perform a specific
formatting task.

The functions are executed in the order they appear in the list when formatting
is triggered."
  :group 'scad-prettify
  :type 'hook)

(defconst scad-prettify--comment-start-re "//\\|/\\*")

(defconst scad-prettify--variable-regex
  "^[[:space:]]*\\([0-9A-Z_a-z]*\\)[[:space:]]*\\(=\\)[[:space:]]*\\([^=][^;]+;\\)"
  "Regex pattern matching SCAD variable declarations.")

(defconst scad-prettify--include-and-use-regexp
  "\\_<\\(include\\|use\\)\\_>[\s]*<\\([.A-Za-z_/][^>]*\\)>"
  "Regular expression matching SCAD include and use statements.")

(defconst scad-prettify--include-regexp
  "\\_<\\(include\\)\\_>[\s]*<\\([.A-Za-z_/][^>]*\\)>"
  "Regular expression matching SCAD include statements.")

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


(defun scad-prettify--all-pass (filters)
  "Create an unary predicate function from FILTERS.
Return t if every one of the provided predicates is satisfied by provided
 argument."
  (lambda (item)
    (not (catch 'found
           (dolist (filter filters)
             (unless (funcall filter item)
               (throw 'found t)))))))
(defun scad-prettify--project-files (project &rest filters)
  "Filter PROJECT files using optional sequential filters.

Argument PROJECT is a representation of the current project to access files
within it.

Optional argument FILTERS is a list of additional filtering predicates applied
to each file within the project."
  (let ((filter
         (if filters
             (scad-prettify--all-pass (append (list #'file-exists-p) filters))
           #'file-exists-p)))
    (seq-filter
     filter
     (project-files project))))
(defun scad-prettify--project-name (&optional project)
  "Return expanded PROJECT root directory path for project or current project.

Optional argument PROJECT specifies the current project object. If not provided,
it attempts to use the current PROJECT by default."
  (when-let* ((project (or project
                           (ignore-errors (project-current))))
              (proj-dir (if (fboundp 'project-root)
                            (project-root (or project))
                          (with-no-warnings
                            (car (project-roots project))))))
    (expand-file-name proj-dir)))
(defun scad-prettify--project-scad-files (project &rest filters)
  "Filter and list PROJECT files with \".scad\" extension, applying FILTERS.

Argument PROJECT is a representation of the current project to access files
within it.

Remaining arguments FILTERS are additional filtering predicates applied to each
file within the project."
  (let ((base-filter (lambda (file)
                       (when-let* ((ext (file-name-extension file)))
                         (string= ext "scad")))))
    (apply #'scad-prettify--project-files
           project
           (if filters
               (append (list base-filter) filters)
             (list base-filter)))))
(defun scad-prettify--backward-whitespace ()
  "Skip backward over whitespace and comments."
  (while (progn
           (skip-chars-backward "\s\t\n")
           (let ((pps (syntax-ppss (point))))
             (and (> (point)
                     (point-min))
                  (cond ((nth 4 pps)
                         (goto-char (nth 8 pps))
                         t)
                        ((looking-back "\\*/" 0)
                         (forward-comment -1)
                         t)))))))

(defun scad-prettify--forward-whitespace ()
  "Advance the point past whitespace and comments in the buffer."
  (let ((pps (syntax-ppss (point)))
        (pos (point)))
    (cond ((nth 4 pps)
           (goto-char (nth 8 pps))
           (forward-comment 1)
           (skip-chars-forward "\s\t\n"))
          (t
           (skip-chars-forward "\s\t\n")))
    (while (looking-at scad-prettify--comment-start-re)
      (forward-comment 1)
      (skip-chars-forward "\s\t\n"))
    (- (point) pos)))


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
          (when (or
                 (not matchers)
                 (not
                  (catch 'mismatch
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
                                                (:inside-comment-or-string
                                                 (scad-prettify--inside-string-or-comment-p))
                                                (:symbol-at-point
                                                 (let ((sym
                                                        (symbol-at-point)))
                                                   (memq sym matcher)))
                                                ((or :inside-comment
                                                     :inside-string)
                                                 (nth (if
                                                          (eq key
                                                              :inside-comment)
                                                          4 3)
                                                      (syntax-ppss
                                                       (point))))
                                                (_
                                                 (save-excursion
                                                   (funcall matcher))))))
                                         (when negatep
                                           (setq result (not result)))
                                         (unless result
                                           (throw 'mismatch t))
                                         (setq pl (funcall
                                                   (if (memq key
                                                             '(:inside-comment-or-string
                                                               :inside-comment
                                                               :inside-string))
                                                       #'cdr
                                                     #'cddr)
                                                   pl))))))))))))))
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
                             end)))))
      (goto-char (point-max))
      (while (re-search-backward
              "}\\|{"
              nil t
              1)
        (unless (scad-prettify--inside-string-or-comment-p)
          (cond ((string= (match-string-no-properties 0) "}")
                 (unless (looking-back "\\(^[\s\t]+\\)\\|\n" 0)
                   (save-excursion
                     (newline-and-indent))))
                ((looking-at "{\\([\s\t]+\\)[\n]")
                 (replace-match "" nil nil nil 1))
                ((and (looking-at "{[^\n]")
                      (not (save-excursion
                             (goto-char (line-end-position))
                             (scad-prettify--inside-string-or-comment-p))))
                 (save-excursion
                   (forward-char 1)
                   (newline-and-indent)))))))))


(defun scad-prettify--top-level-p (&optional pos)
  "Determine if POS is at top-level, not in a comment or string.

Optional argument POS is a buffer position, defaulting to the current point."
  (let* ((pps (syntax-ppss (or pos (point))))
         (level (car pps)))
    (and (zerop level)
         (not (or (nth 4 pps)
                  (nth 3 pps))))))

(defun scad-prettify-align-variables ()
  "Align variable assignments by padding spaces for consistent formatting."
  (let ((longest-len)
        (last-var-end))
    (save-excursion
      (save-match-data
        (goto-char (point-min))
        (while
            (when (and (re-search-forward scad-prettify--variable-regex nil t 1)
                       (scad-prettify--top-level-p)
                       (save-excursion
                         (scad-prettify--top-level-p (match-beginning 0))))
              (let ((var-name (match-string-no-properties 1)))
                (setq last-var-end (match-end 0))
                (setq longest-len (max (or longest-len 0)
                                       (length var-name))))))
        (when (and last-var-end longest-len)
          (goto-char last-var-end)
          (while
              (re-search-backward scad-prettify--variable-regex nil t 1)
            (when (and (scad-prettify--top-level-p)
                       (save-excursion
                         (scad-prettify--top-level-p
                          (match-end 0))))
              (let ((var-name (match-string-no-properties 1))
                    (value (match-string-no-properties 3))
                    (start (match-beginning 0))
                    (end (match-end 0)))
                (setq value (string-trim-left value))
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
                                               (if (string-prefix-p spaces line)
                                                   line
                                                 (concat spaces
                                                         (string-trim line))))
                                             lines
                                             "\n"))
                      (setq rep (concat rep first-line "\n" lines))))
                  (delete-region start end)
                  (save-excursion
                    (insert rep)))))))))))


(defun scad-prettify--goto-line (line)
  "Move cursor to line LINE."
  (goto-char (point-min))
  (forward-line (1- line)))

(defun scad-prettify--apply-rcs-patch (patch-buffer)
  "Apply an RCS-formatted diff from PATCH-BUFFER to the current buffer."
  (let ((target-buffer (current-buffer))
        (line-offset 0))
    (save-excursion
      (with-current-buffer patch-buffer
        (goto-char (point-min))
        (while (not (eobp))
          (unless (looking-at "^\\([ad]\\)\\([0-9]+\\) \\([0-9]+\\)")
            (error
             "Invalid rcs patch or internal error in scad-prettify--apply-rcs-patch"))
          (forward-line)
          (let ((action (match-string 1))
                (from (string-to-number (match-string 2)))
                (len  (string-to-number (match-string 3))))
            (cond ((equal action "a")
                   (let ((start (point)))
                     (forward-line len)
                     (let ((text (buffer-substring start (point))))
                       (with-current-buffer target-buffer
                         (setq line-offset (- line-offset len))
                         (goto-char (point-min))
                         (forward-line (- from len line-offset))
                         (insert text)))))
                  ((equal action "d")
                   (with-current-buffer target-buffer
                     (scad-prettify--goto-line (- from line-offset))
                     (setq line-offset (+ line-offset len))
                     (let ((beg (point)))
                       (forward-line len)
                       (delete-region (point) beg))))
                  (t
                   (error
                    "Invalid rcs patch or internal error in scad-prettify--apply-rcs-patch")))))))))

(defun scad-prettify--apply-patch (beg end replacement)
  "Apply a diff patch to the current buffer.

Argument BEG is the beginning position in the buffer where the patch will be
applied.

Argument END is the ending position in the buffer where the patch will be
applied.

Argument REPLACEMENT is the string that will replace the text between BEG and
END."
  (let* ((outputfile (make-temp-file "scad-prettify" nil "el"))
         (patchbuf (get-buffer-create "*scad prettify patch*"))
         (coding-system-for-read 'utf-8)
         (coding-system-for-write 'utf-8)
         (col (current-column)))
    (unwind-protect
        (save-restriction
          (write-region replacement nil outputfile nil 'silent)
          (with-current-buffer patchbuf
            (erase-buffer))
          (progn
            (call-process-region beg
                                 end "diff" nil patchbuf nil "-n"
                                 "--strip-trailing-cr" "-"
                                 outputfile)
            (narrow-to-region beg end)
            (scad-prettify--apply-rcs-patch patchbuf)
            (move-to-column col t)))
      (kill-buffer patchbuf)
      (delete-file outputfile))))



(defun scad-prettify-indent-buffer-ignore-comments ()
  "Indent each top-level expression in the buffer, skipping comments and includes."
  (goto-char (point-min))
  (while
      (progn
        (scad-prettify--forward-whitespace)
        (cond ((looking-at scad-prettify--include-and-use-regexp)
               (forward-line 1))
              ((eobp)
               nil)
              (t
               (when-let* ((start (point))
                           (end
                            (when (scad-prettify--forward-sexp)
                              (point))))
                 (when (looking-at ";")
                   (forward-char 1)
                   (setq end (1+ end)))
                 (indent-region start
                                end)
                 t))))))

(defun scad-prettify--forward-sexp ()
  "Move forward over a SCAD expression, handling comments and whitespace."
  (let ((max-pos (point-max))
        (start (point))
        (end))
    (while
        (and (not end)
             (progn
               (scad-prettify--forward-whitespace)
               (when (> max-pos (point))
                 (cond ((looking-at scad-prettify--include-and-use-regexp)
                        (end-of-line 1)
                        (setq end (point)))
                       (t
                        (pcase (char-to-string (char-after (point)))
                          ("{" (forward-sexp)
                           nil)
                          ((or "(" "[" "\"")
                           (forward-sexp)
                           (> max-pos
                              (point)))
                          ("/"
                           (if
                               (not (looking-at scad-prettify--comment-start-re))
                               (progn (forward-char 1)
                                      (> (point-max)
                                         (point)))
                             (scad-prettify--forward-whitespace)
                             (> max-pos
                                (point))))
                          ((or ")" "]" "}" "," ";") nil)
                          (_ (> (skip-chars-forward "^;,)]([\"/") 0)))))))))
    (< start (or end (point)))))


(defun scad-prettify--forward-symbol (&optional definition)
  "Return symbol at point, moving forward over allowed identifier characters.

Optional argument DEFINITION is non-nil to parse a definition symbol;
defaults to nil."
  (let ((case-fold-search t))
    (cond (definition
           (when-let* ((pos (and (looking-at "[a-z_]")
                                 (point))))
             (buffer-substring-no-properties pos
                                             (+ pos
                                                (skip-chars-forward
                                                 "a-zA-Z_0-9")))))
          (t
           (when-let* ((pos (and (looking-at "[$a-z_]")
                                 (point))))
             (buffer-substring-no-properties pos
                                             (+ pos
                                                (skip-chars-forward
                                                 "a-z_0-9$"))))))))

(defun scad-prettify--align-args-at-point ()
  "Align arguments after point by inserting newlines and removing trailing commas."
  (scad-prettify--forward-sexp)
  (let ((last-comma-pos))
    (while (looking-at ",")
      (setq last-comma-pos (point))
      (forward-char 1)
      (let ((curr-line (line-number-at-pos (point))))
        (scad-prettify--forward-whitespace)
        (if (looking-at ")\\|\\]")
            (delete-region last-comma-pos
                           (if (string-empty-p
                                (string-trim (buffer-substring-no-properties
                                              (1+ last-comma-pos)
                                              (point))))
                               (point)
                             (1+ last-comma-pos)))
          (when (= curr-line (line-number-at-pos (point)))
            (newline-and-indent))
          (scad-prettify--forward-sexp))))))



(defun scad-prettify-align-args ()
  "Align function call arguments by inserting newlines and removing commas."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (while
          (let ((case-fold-search t))
            (re-search-forward "#?\\([a-z]\\([a-z_0-9$]*\\)\\)[\s\t]*("
                               nil
                               t 1))
        (let ((symb (match-string-no-properties 1)))
          (unless (or (scad-prettify--inside-string-or-comment-p)
                      (looking-at ")")
                      (save-excursion
                        (forward-char -1)
                        (let ((line-start (line-number-at-pos (point)))
                              (line-end))
                          (condition-case nil
                              (progn
                                (forward-sexp 1)
                                (setq line-end
                                      (line-number-at-pos (point))))
                            (error nil))
                          (or (not line-end)
                              (and (equal line-end line-start)
                                   (> fill-column (current-column)))))))
            (when (equal symb "translate")
              (forward-char 1)
              (scad-prettify--forward-whitespace))
            (scad-prettify--align-args-at-point)))))))

(defun scad-prettify-sort-imports ()
  "Sort and deduplicate include/use statements, replacing the import block."
  (save-excursion
    (save-match-data
      (goto-char (point-min))
      (let ((start)
            (end)
            (imports))
        (while
            (when (re-search-forward scad-prettify--include-and-use-regexp nil t
                                     1)
              (or (scad-prettify--inside-string-or-comment-p)
                  (progn (setq start (line-beginning-position))
                         (setq imports
                               (push
                                (string-trim
                                 (buffer-substring-no-properties start
                                                                 (line-end-position)))
                                imports))
                         nil))))
        (when start
          (while (and (zerop (forward-line))
                      (progn
                        (let ((imp-beg (point)))
                          (scad-prettify--forward-whitespace)
                          (when (looking-at
                                 scad-prettify--include-and-use-regexp)
                            (push
                             (string-trim
                              (buffer-substring-no-properties imp-beg
                                                              (line-end-position)))
                             imports))))))
          (scad-prettify--backward-whitespace)
          (setq end (point)))
        (when (and imports start end)
          (let* ((re-start (concat "^" scad-prettify--include-and-use-regexp))
                 (sorted (seq-sort-by (lambda (imp)
                                        (if (or (string-prefix-p "include" imp)
                                                (string-prefix-p "use" imp))
                                            imp
                                          (let ((item (car (seq-drop-while
                                                            (lambda (it)
                                                              (not
                                                               (string-match-p
                                                                re-start it)))
                                                            (split-string imp
                                                                          "[\n\r\f]"
                                                                          t)))))
                                            item)))
                                      #'string<
                                      (delete-dups
                                       imports)))
                 (rep (with-temp-buffer
                        (insert (string-join sorted "\n"))
                        (when (re-search-backward
                               scad-prettify--include-regexp
                               nil t 1)
                          (goto-char (line-end-position))
                          (insert "\n"))
                        (buffer-string))))
            (replace-region-contents start end
                                     (lambda ()
                                       rep))
            sorted))))))


(defun scad-prettify-sort-project-imports ()
  "Sort imports in all .scad files in the current project, saving changes."
  (interactive)
  (let* ((project (ignore-errors (project-current)))
         (files (scad-prettify--project-scad-files project))
         (project-dir (if project
                          (scad-prettify--project-name project)
                        default-directory))
         (file))
    (sit-for 0.01)
    (while (setq file (pop files))
      (setq file (expand-file-name file))
      (let ((shortname (substring-no-properties
                        file
                        (length project-dir)))
            (buff (get-file-buffer file)))
        (message "Checking %s" shortname)
        (if (buffer-live-p buff)
            (with-current-buffer buff
              (let ((buff-modified (buffer-modified-p)))
                (scad-prettify-sort-imports)
                (unless buff-modified
                  (save-buffer))))
          (with-temp-buffer
            (insert-file-contents file)
            (let ((scad-mode-hook nil))
              (scad-mode))
            (when (scad-prettify-sort-imports)
              (write-region nil nil file nil nil))))
        (sit-for 0.01)))))



;;;###autoload
(defun scad-prettify-buffer ()
  "Format the buffer by adjusting braces, aligning variables, and indenting."
  (interactive)
  (let* ((buff (current-buffer))
         (result (with-temp-buffer
                   (insert-buffer-substring buff)
                   (let ((scad-mode-hook nil))
                     (scad-mode)
                     (hack-local-variables)
                     (let ((indent-tabs-mode nil)
                           (inhibit-message t))
                       (scad-prettify--format-by-regex)
                       (run-hooks 'scad-prettify-formatters)))
                   (buffer-string))))
    (unless (string= (buffer-substring-no-properties (point-min)
                                                     (point-max))
                     result)
      (scad-prettify--apply-patch
       (point-min)
       (point-max) result))))

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