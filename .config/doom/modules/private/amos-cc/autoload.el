;;; lang/cc/autoload.el -*- lexical-binding: t; -*-

;;;###autoload
(add-to-list 'auto-mode-alist '("\\.cl\\'" . opencl-mode))


;;
;; Library

;;;###autoload
(defun +cc-sp-point-is-template-p (id action context)
  "Return t if point is in the right place for C++ angle-brackets."
  (and (sp-in-code-p id action context)
       (cond ((eq action 'insert)
              (sp-point-after-word-p id action context))
             ((eq action 'autoskip)
              (/= (char-before) 32)))))

;;;###autoload
(defun +cc-sp-point-after-include-p (id action context)
  "Return t if point is in an #include."
  (and (sp-in-code-p id action context)
       (save-excursion
         (goto-char (line-beginning-position))
         (looking-at-p "[ 	]*#include[^<]+"))))

;;;###autoload
(defun +cc-c++-lineup-inclass (langelem)
  "Indent inclass lines one level further than access modifier keywords."
  (and (eq major-mode 'c++-mode)
       (or (assoc 'access-label c-syntactic-context)
           (save-excursion
             (save-match-data
               (re-search-backward
                "\\(?:p\\(?:ublic\\|r\\(?:otected\\|ivate\\)\\)\\)"
                (c-langelem-pos langelem) t))))
       '++))

;;;###autoload
(defun +cc-lineup-arglist-close (langlem)
  "Line up the closing brace in an arglist with the opening brace IF cursor is
preceded by the opening brace or a comma (disregarding whitespace in between)."
  (when (save-excursion
          (save-match-data
            (skip-chars-backward " \t\n" (c-langelem-pos langelem))
            (memq (char-before) (list ?, ?\( ?\;))))
    (c-lineup-arglist langlem)))

;;;###autoload
(defun +cc-lineup-comments (langelem)
  (save-excursion
    (let* ((here (point))
           (prefixlen (progn (back-to-indentation)
                             (if (looking-at c-current-comment-prefix)
                                 (- (match-end 0) (point))
                               0)))
           (starterlen
            (max (save-excursion
                   (goto-char (1+ (c-langelem-pos langelem)))
                   (if (and (match-string 0)
                            (looking-at (regexp-quote (match-string 0))))
                       (- (match-end 0) (match-beginning 0))
                     0))
                 (save-excursion
                   (goto-char (c-langelem-pos langelem))
                   (looking-at comment-start-skip)
                   (- (or (match-end 1)
                          (save-excursion
                            (goto-char (match-end 0))
                            (skip-chars-backward " \t")
                            (point)))
                      (point)
                      1)))))
      (if (and (> starterlen 10) (zerop prefixlen))
          (vector (current-column))
        (while
            (progn
              (forward-line -1)
              (back-to-indentation)
              (and (> (point) (c-langelem-pos langelem))
                   (looking-at "[ \t]*$"))))
        (if (>= (c-langelem-pos langelem) (point))
            (if (zerop prefixlen)
                (progn
                  (looking-at comment-start-skip)
                  (goto-char (match-end 0))
                  (vector (current-column)))
              (goto-char (+ (c-langelem-pos langelem) starterlen 1))
              (vector (- (current-column) prefixlen)))
          (when (or (not (looking-at c-current-comment-prefix))
                    (eq (match-beginning 0) (match-end 0)))
            (goto-char here)
            (back-to-indentation)
            (if (looking-at (concat "\\(" c-current-comment-prefix "\\)\\*/"))
                (goto-char (c-langelem-pos langelem))
              (while (and (zerop (forward-line -1))
                          (looking-at "^[ \t]*$")))
              (back-to-indentation)
              (if (< (point) (c-langelem-pos langelem))
                  (goto-char (c-langelem-pos langelem)))))
          (vector (current-column)))))))

(defun +cc-inside-class-enum-p (pos)
  "Checks if POS is within the braces of a C++ \"enum class\"."
  (ignore-errors
    (save-excursion
      (goto-char pos)
      (backward-sexp 1)
      (or (looking-back "enum\\s-+" 0)
          (looking-back "enum\\s-+class\\s-+" 0)
          (looking-back "enum\\s-+class\\s-+\\S-+\\s-*:\\s-*" 0)))))

;;;###autoload
(defun +cc-lineup-enum-class (langelem)
  (if (+cc-inside-class-enum-p (c-langelem-pos langelem))
      0
    (c-lineup-topmost-intro-cont langelem)))

;;;###autoload
(defun +cc-lineup-enum-class-closing-brace (langelem)
  (if (+cc-inside-class-enum-p (c-langelem-pos langelem))
      '-
    '+))

;;;###autoload
(defun +cc-llvm-lineup-statement (langelem)
  (let ((in-assign (c-lineup-assignments langelem)))
    (if (not in-assign)
        '++
      (aset in-assign 0
            (+ (aref in-assign 0)
               (* 2 c-basic-offset)))
      in-assign)))

(defun +cc--re-search-for (regexp)
  (save-excursion
    (save-restriction
      (save-match-data
        (widen)
        (goto-char (point-min))
        (re-search-forward regexp magic-mode-regexp-match-limit t)))))

;;;###autoload
(defun +cc-c-c++-objc-mode ()
  "Uses heuristics to detect `c-mode', `objc-mode' or `c++-mode'.

1. Checks if there are nearby cpp/cc/m/mm files with the same name.
2. Checks for ObjC and C++-specific keywords and libraries.
3. Falls back to `+cc-default-header-file-mode', if set.
4. Otherwise, activates `c-mode'.

This is meant to replace `c-or-c++-mode' (introduced in Emacs 26.1), which
doesn't support specification of the fallback mode and whose heuristics are
simpler."
  (let ((base (file-name-sans-extension (buffer-file-name (buffer-base-buffer)))))
    (cond ((file-exists-p! (or (concat base ".cpp")
                               (concat base ".cc")))
           (c++-mode))
          ((or (file-exists-p! (or (concat base ".m")
                                   (concat base ".mm")))
               (+cc--re-search-for
                (concat "^[ \t\r]*\\(?:"
                        "@\\(?:class\\|interface\\|property\\|end\\)\\_>"
                        "\\|#import +<Foundation/Foundation.h>"
                        "\\|[-+] ([a-zA-Z0-9_]+)"
                        "\\)")))
           (objc-mode))
          ((+cc--re-search-for
            (let ((id "[a-zA-Z0-9_]+") (ws "[ \t\r]+") (ws-maybe "[ \t\r]*"))
              (concat "^" ws-maybe "\\(?:"
                      "using" ws "\\(?:namespace" ws "std;\\|std::\\)"
                      "\\|" "namespace" "\\(?:" ws id "\\)?" ws-maybe "{"
                      "\\|" "class"     ws id ws-maybe "[:{\n]"
                      "\\|" "template"  ws-maybe "<.*>"
                      "\\|" "#include"  ws-maybe "<\\(?:string\\|iostream\\|map\\)>"
                      "\\)")))
           (c++-mode))
          ((functionp +cc-default-header-file-mode)
           (funcall +cc-default-header-file-mode))
          ((c-mode)))))

(defun +cc-resolve-include-paths ()
  (cl-loop with path = (or buffer-file-name default-directory)
           for dir in +cc-default-include-paths
           if (file-name-absolute-p dir)
           collect dir
           else if (projectile-locate-dominating-file path dir)
           collect (expand-file-name dir it)))

;;;###autoload
(defun +cc-append-comment-line ()
  (interactive)
  (evil-ret 1)
  (if (eq (car (car (c-guess-basic-syntax))) 'c)
      (if (string= (save-excursion (previous-line 1) (buffer-substring-no-properties (point-at-bol) (point-at-eol))) " * ")
          (progn
            (delete-backward-char 2)
            (insert "/"))
        (insert "* ")))
  (indent-according-to-mode))

;;
;; Hooks

;;;###autoload
(defun +cc|fontify-constants ()
  "Better fontification for preprocessor constants"
  (when (memq major-mode '(c-mode c++-mode))
    (font-lock-add-keywords
     nil '(("\\<[A-Z]*_[0-9A-Z_]+\\>" . font-lock-constant-face)
           ("\\<[A-Z]\\{3,\\}\\>"  . font-lock-constant-face))
     t)))
