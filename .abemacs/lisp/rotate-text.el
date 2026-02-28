;;; rotate-text.el --- cycle through words, symbols and patterns  -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2009 Nikolaj Schumacher
;;
;; Author: Nikolaj Schumacher <bugs * nschum de>
;; Version: 0.1
;; Keywords: abbrev, convenience, matching
;; URL: http://nschum.de/src/emacs/rotate-text/
;; Compatibility: GNU Emacs 22.x, GNU Emacs 23.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; rotate-text allows you cycle through commonly interchanged text with a single
;; keystroke.  For example, you can toggle between "frame-width" and
;; "frame-height", between "public", "protected" and "private" and between
;; "variable1", "variable2" through "variableN".
;;
;; Add the following to your .emacs:
;;
;; (add-to-list 'load-path "/path/to/rotate-text")
;; (autoload 'rotate-text "rotate-text" nil t)
;; (autoload 'rotate-text-backward "rotate-text" nil t)
;;
;; Customize the variables `rotate-text-patterns', `rotate-text-symbols' and
;; `rotate-text-words'.  You can make buffer-local additions in
;; `rotate-text-local-patterns', `rotate-text-local-symbols' and
;; `rotate-text-local-words'.
;;
;; Use the commands `rotate-text' and `rotate-text-backward' to rotate the
;; text.
;;
;;; Change Log:
;;
;; 2009-04-13 (0.1)
;;    Initial release.
;;
;;; Code:

(eval-when-compile (require 'cl))

(add-to-list 'debug-ignored-errors "^Nothing to rotate$")

(defgroup rotate-text nil
  "Cycle through words, symbols and patterns."
  :group 'abbrev
  :group 'convenience
  :group 'matching)

(defcustom rotate-text-patterns
  '(("\\_<[^-]\\(\\sw\\|\\s_\\)*[0-9]+" rotate-text-increment-number-in-symbol)
    ("-?0x?[0-9a-fA-F]+" rotate-text-increment-hex-number)
    ("-?[0-9]+" rotate-text-increment-number))
  "*Patterns and functions to rotate them.
Each entry is a list.  Its first element should be the regular expression to
replace, the second element is a function.  When rotating, it is called with the
matching text and an integer determining the rotation amount and direction."
  :group 'rotate-text
  :type '(repeat (list (string :tag "Regular expression")
                       (function :tag "Rotation function"))))

(defcustom rotate-text-symbols '(("private" "protected" "public"))
  "*List of symbol names to rotate.
Each element is a list of symbols that should be cycled through."
  :group 'rotate-text
  :type '(repeat (repeat :tag "Rotation group" (string :tag "Symbol"))))

(defcustom rotate-text-words '(("width" "height")
                               ("left" "right" "top" "bottom"))
  "*List of words to rotate.
Each element is a list of words that should be cycled through.  Individual
segments in symbol names are recognized as words, i.e. windowWidth can be
replaced with windowHeight.
All entries must be in lower case. The case is determined by the rotated
text."
  :group 'rotate-text
  :type '(repeat (repeat :tag "Rotation group" (string :tag "Word"))))

(defvar rotate-text-local-patterns nil
  "*Buffer local additions to `rotate-text-patterns'.")
(make-variable-buffer-local 'rotate-text-local-patterns)

(defvar rotate-text-local-symbols nil
  "*Buffer local additions to `rotate-text-symbols'.")
(make-variable-buffer-local 'rotate-text-local-symbols)

(defvar rotate-text-local-words nil
  "*Buffer local additions to `rotate-text-words'.")
(make-variable-buffer-local 'rotate-text-local-words)

;;; numbers ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rotate-text-increment-number (original arg &optional minimum)
  (number-to-string (max (+ (string-to-number original) arg)
                         (or minimum most-negative-fixnum))))

(defun rotate-text-increment-hex-number (original arg)
  (when (string-match "\\`-?\\(0x\\)" original)
    (setq original (replace-match "" t t original 1)))
  (let ((result (+ (string-to-number original 16) arg)))
    (format "%s0x%x" (if (< result 0) "-" "") (abs result))))

(defun rotate-text-increment-number-in-symbol (original arg)
  (when (string-match "[0-9]+" original)
    (replace-match (rotate-text-increment-number (match-string 0 original)
                                                 arg 0)
                   t t original)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun rotate-text-replacement (replacements original dir)
  "Find the replacement for ORIGINAL in REPLACEMENTS."
  (save-match-data
    (if (functionp (car replacements))
        ;; function
        (if (and (< dir 0) (functionp (cadr replacements)))
            (funcall (cadr replacements) original (- dir))
          (funcall (car replacements) original dir))
      ;; list
      (let ((rest-pattern (member original replacements)))
        (when rest-pattern
          (car (nthcdr (mod (- dir (length rest-pattern)) (length replacements))
                       replacements)))))))

(defun rotate-text-match-at-point (regexp)
  (save-excursion
    (let ((pos (point)))
      (goto-char (point-at-bol))
      (catch 'match
        (while (re-search-forward regexp (1+ (point-at-eol)) t)
          (and (>= pos (match-beginning 0))
               (<= pos (match-end 0))
               (throw 'match (match-string-no-properties 0))))))))

(defun rotate-text-symbol-at-point ()
  "Rotate the symbol at point."
  (rotate-text-match-at-point "\\_<\\(\\s_\\|\\sw\\)+\\_>"))

(defun rotate-text-word-at-point ()
  "Rotate the word at point."
  (let ((case-fold-search nil))
    (or (rotate-text-match-at-point "\\(\\<\\|[[:upper:]]\\)[[:lower:]]+")
        (rotate-text-match-at-point "\\<[[:upper:]]+"))))

(defun rotate-text-match-case (original new)
  "Match the case of ORIGINAL in NEW."
  (let ((case-fold-search nil))
    (save-match-data
      (cond
       ((string-match "\\`[[:upper:]][[:lower:]]" original) (capitalize new))
       ((string-match "\\`[[:upper:]][[:upper:]]" original) (upcase new))
       (t new)))))

(defvar rotate-text-last-offset nil)

;;;###autoload
(defun rotate-text (arg &optional default-string com-symbols com-words com-patterns)
  "Rotate the text at point. If there is nothing to rotate at point and DEFAULT-STRING is non-nil,
DEFAULT-STRING is inserted at point.

COM-SYMBOLS, COM-WORDS and COM-PATTERNS are per-command addition to `rotate-text-symbols',
`rotate-text-words' and `rotate-text-patterns', respectively."
  (interactive (list (if (consp current-prefix-arg)
                         -1
                       (prefix-numeric-value current-prefix-arg))))
  (let ((pos (point))
        (offset 0)
        match replacement)
    (or ;; symbols
        (when (setq match (rotate-text-symbol-at-point))
          (dolist (symbols (append com-symbols rotate-text-local-symbols
                                   rotate-text-symbols))
            (when (setq replacement
                        (rotate-text-replacement symbols match arg))
              (cl-return t))))
        ;; words
        (when (setq match (rotate-text-word-at-point))
          (dolist (words (append com-words rotate-text-local-words
                                 rotate-text-words))
            (when (setq replacement
                        (rotate-text-replacement words (downcase match) arg))
              (setq replacement (rotate-text-match-case match replacement))
              (cl-return t))))
        ;; regexp
        (dolist (pattern (append com-patterns rotate-text-local-patterns
                                 rotate-text-patterns))
          (when (setq match (rotate-text-match-at-point (car pattern)))
            (setq replacement (rotate-text-replacement (cdr pattern) match arg))
            (cl-return t))))

    (if (not replacement)
        (progn (unless default-string
                 (error "Nothing to rotate"))
               (insert default-string)
               (setq rotate-text-last-offset nil))
      
      (progn
        (unless (and rotate-text-last-offset
                     (eq last-command this-command))
          (setq rotate-text-last-offset
                (if (eq pos (match-end 0))
                    'end
                  (- pos (match-beginning 0)))))

        (replace-match replacement nil t)

        (goto-char (if (eq rotate-text-last-offset 'end)
                       (match-end 0)
                     (min (+ (match-beginning 0) rotate-text-last-offset)
                          (match-end 0))))))))

    

;;;###autoload
(defun rotate-text-backward (arg &optional default-string com-symbols com-words com-patterns)
  "Rotate the text at point backwards. If there is nothing to rotate at point and DEFAULT-STRING is non-nil,
DEFAULT-STRING is inserted at point.

COM-SYMBOLS, COM-WORDS and COM-PATTERNS are per-command addition to `rotate-text-symbols',
`rotate-text-words' and `rotate-text-patterns', respectively."
  (interactive (list (if (consp current-prefix-arg)
                         -1
                       (prefix-numeric-value current-prefix-arg))))
  (rotate-text (- arg) default-string com-symbols com-words com-patterns))

(provide 'rotate-text)
;;; rotate-text.el ends here
