;;; lang/cc/config.el --- c, c++, and obj-c -*- lexical-binding: t; -*-

(def-package! cc-mode
  :commands (c-mode c++-mode objc-mode java-mode)
  :mode ("\\.mm" . objc-mode) ("\\.h\\'" . c++-mode)
  :preface
  (defun +cc-c++-header-file-p ()
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name) "h")
         (or (file-exists-p (expand-file-name
                             (concat (file-name-sans-extension buffer-file-name)
                                     ".cpp"))))))

  (defun +cc-objc-header-file-p ()
    (and buffer-file-name
         (equal (file-name-extension buffer-file-name) "h")
         (re-search-forward "@\\<interface\\>" magic-mode-regexp-match-limit t)))

  (push (cons #'+cc-c++-header-file-p  'c++-mode)  magic-mode-alist)
  (push (cons #'+cc-objc-header-file-p 'objc-mode) magic-mode-alist)

  :init
  (setq-default c-basic-offset tab-width)

  :config
  (set-electric! '(c-mode c++-mode objc-mode java-mode)
    :chars '(?{ ?\n ?}))

  (defun +amos-append-comment-line ()
    (interactive)
    (newline-and-indent)
    (if (eq (car (car (c-guess-basic-syntax))) 'c)
        (insert "* "))
    (indent-according-to-mode))

  ;; TODO count doesn't work, last line broken
  (defun +amos-evil-open-below (count)
    "Insert a new line below point and switch to Insert state.
The insertion will be repeated COUNT times."
    (interactive "p")
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step))
    (push (point) buffer-undo-list)
    (evil-insert-newline-below)
    (setq evil-insert-count count
          evil-insert-lines t
          evil-insert-vcount nil)
    (evil-insert-state 1)
    (if (eq (car (car (c-guess-basic-syntax))) 'c)
        (insert "* "))
    (when evil-auto-indent
      (indent-according-to-mode)))

  ;; TODO count doesn't work, first line broken
  (defun +amos-evil-open-above (count)
    "Insert a new line above point and switch to Insert state.
The insertion will be repeated COUNT times."
    (interactive "p")
    (unless (eq evil-want-fine-undo t)
      (evil-start-undo-step))
    (evil-insert-newline-above)
    (setq evil-insert-count count
          evil-insert-lines t
          evil-insert-vcount nil)
    (evil-insert-state 1)
    (if (eq (car (car (c-guess-basic-syntax))) 'c)
        (insert "* "))
    (when evil-auto-indent
      (indent-according-to-mode)))

  (defun +amos/cc-better-semicolon ()
    (interactive)
    (if (and (eolp) (looking-back ";"))
        (funcall-interactively (key-binding (kbd "RET")))
      (insert ";")))
  ;; Smartparens and cc-mode both try to autoclose angle-brackets intelligently.
  ;; The result isn't very intelligent (causes redundant characters), so just do
  ;; it ourselves.
  (map! (:map (c-mode-map c++-mode-map)
          "<" nil
          :i ">"        #'+cc/autoclose->-maybe
          :i "RET"      #'+amos-append-comment-line
          :i ";"        #'+amos/cc-better-semicolon
          :n "C-e"      #'+amos/maybe-add-end-of-statement
          :n "o"        #'+amos-evil-open-below
          :n "O"        #'+amos-evil-open-above
          :n "gh"       #'ccls-call-hierarchy
          :n "gR"       #'ccls/callers
          :n "gt"       #'cquery-member-hierarchy
          :n "ge"       #'lsp-execute-code-action
          "C-c i"       #'ccls/includes
          "C-c I"       (lambda! (ccls/includes t))))

  ;;; Style/formatting
  ;; C/C++ style settings
  (c-toggle-electric-state -1)
  (c-toggle-auto-newline -1)

  (defun my-c-lineup-arglist-lambda (langelem)
    "Line up lambda."
    (save-excursion
      (back-to-indentation)
      (when (looking-at "{") '+)))

  (c-set-offset 'substatement-open     0) ; don 't indent brackets
  (c-set-offset 'innamespace           0)
  (c-set-offset 'inline-open           0)
  (c-set-offset 'block-open            0)
  (c-set-offset 'inlambda              0)
  (c-set-offset 'statement-block-intro '+)
  (c-set-offset 'case-label            '+)
  (c-set-offset 'access-label          '-)
  (c-set-offset 'arglist-intro         '+)
  ;; (c-set-offset 'statement-cont        '(c-lineup-assignments +))
  (c-set-offset 'statement-cont        '+)
  (c-set-offset 'arglist-close         #'c-lineup-arglist)

  ;; (c-set-offset 'defun-block-intro '(my-c-lineup-arglist-lambda '+))
  ;; (c-set-offset 'arglist-cont-nonempty '(my-c-lineup-arglist-lambda c-lineup-arglist))
  (c-set-offset 'arglist-cont-nonempty '+)

  (defun inside-class-enum-p (pos)
    "Checks if POS is within the braces of a C++ \"enum class\"."
    (ignore-errors
      (save-excursion
        (goto-char pos)
        (backward-sexp 1)
        (or (looking-back "enum\\s-+")
            (looking-back "enum\\s-+class\\s-+")
            (looking-back "enum\\s-+class\\s-+\\S-+\\s-*:\\s-*")))))

  (defun align-enum-class (langelem)
    (if (inside-class-enum-p (c-langelem-pos langelem))
        0
      (c-lineup-topmost-intro-cont langelem)))

  (defun align-enum-class-closing-brace (langelem)
    (if (inside-class-enum-p (c-langelem-pos langelem))
        0
      '+))

  (c-set-offset 'brace-list-open #'align-enum-class)
  (c-set-offset 'brace-list-intro '+)
  (c-set-offset 'brace-list-close #'align-enum-class-closing-brace)

  (defun +amos-c-lineup-C-comments (langelem)
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
  (c-set-offset 'c #'+amos-c-lineup-C-comments)

  (setq c-noise-macro-names '("constexpr"))
  ;; Indent privacy keywords at same level as class properties
  ;; (c-set-offset 'inclass #'+cc-c-lineup-inclass)

  (add-hook 'c-mode-common-hook #'(lambda () (modify-syntax-entry ?_ "w")))

  ;; Improve indentation of inline lambdas in C++11
  ;; (advice-add #'c-lineup-arglist :around #'+cc*align-lambda-arglist)

  ;;; Keybindings
  ;; Completely disable electric keys because it interferes with smartparens and
  ;; custom bindings. We'll do this ourselves.
  ;; (setq c-tab-always-indent t
  ;;       c-electric-flag nil)
  ;; (dolist (key '("#" "{" "}" "/" "*" ";" "," ":" "(" ")"))
  ;; (dolist (key '("/" "*"))
  ;;   (define-key c-mode-base-map key nil))

  ;; ...and leave it to smartparens
  (after! smartparens
    (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
      (sp-local-pair "/*" "*/" :post-handlers '(("||\n[i]" "RET") ("| " "SPC") (" ||\n[i]" "*"))))))
;; Doxygen blocks

(def-package! cmake-mode
  :mode
  (("/CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode)))

(set-rotate-patterns! 'c++-mode
  :symbols '(("public" "protected" "private")
             ("class" "struct")))

(def-package! disaster :commands disaster)

(def-package! cuda-mode :mode "\\.cuh?$")

(def-package! opencl-mode :mode "\\.cl$")

(def-package! demangle-mode
  :commands demangle-mode
  :init (add-hook 'llvm-mode-hook #'demangle-mode))

(def-package! clang-format
  :commands clang-format-buffer clang-format)

(defun +amos/add-include (h &rest others)
  "Add an #include line for `h' near top of file, avoiding duplicates."
  (interactive "M#include: ")
  (dolist (header (cons h others))
    (let ((incl (format "#include <%s>" header)))
      (save-excursion
        (if (search-backward incl nil t)
            nil
          (when (search-backward "#include" nil 'stop-at-top)
            (forward-line)
            (beginning-of-line))
          (insert incl)
          (newline))))))

(add-hook! (c-mode c++-mode) (flycheck-mode +1) (eldoc-mode -1))
(add-hook! 'c++-mode-hook #'modern-c++-font-lock-mode)
(set-company-backend!
  '(c-mode c++-mode objc-mode)
  'company-dabbrev-code)

(def-package! ccls
  :after lsp-mode
  :init
  (setq
   ccls-project-root-matchers
   '(ccls-project-roots-matcher ".ccls" ".cquery" projectile-project-root "compile_commands.json")
   ;; ccls-sem-highlight-method 'font-lock)
   ;; ccls-sem-highlight-method 'overlay)
   ccls-sem-highlight-method nil)

  (defun +amos-lsp-find-custom (kind request &optional param)
    (let* ((input (symbol-at-point))
           (xrefs
            (-some->> (lsp--send-request (lsp--make-request
                                          request
                                          (or param (lsp--text-document-position-params))))
                      ;; Language servers may return a single LOCATION instead of a sequence of them.
                      (lsp-ui-peek--to-sequence)
                      (lsp--locations-to-xref-items)
                      (-filter 'identity))))
      (unless xrefs
        (user-error "No %s found for: %s" (symbol-name kind) input))
      (+amos-ivy-xref xrefs kind)))

  (defun ccls/includes (&optional force)
    (interactive)
    (if (ccls--is-ccls-buffer)
        (let ((x (intern (concat (doom-project-root) "--includes"))))
          (unless (and (boundp x) (not force))
            (setq x
                  (lsp--send-request
                   (lsp--make-request "$ccls/includes"))))
          (ivy-read "Include: " x :action #'+amos/add-include))))

  (defun ccls/diagnostic ()
    (interactive)
    (if (ccls--is-ccls-buffer)
        (lsp--send-notification
         (lsp--make-notification "$ccls/diagnostic"
                                 `(:textDocument ,(lsp--text-document-identifier))))))

  (defun ccls/inheritances ()
    (interactive)
    (if (ccls--is-ccls-buffer)
        (let* ((input (symbol-at-point))
               (xrefs
                (-some->> (lsp--send-request (lsp--make-request "$ccls/inheritances"
                                                                `(:textDocument ,(lsp--text-document-identifier)
                                                                                :position ,(lsp--cur-position))))
                          (lsp-ui-peek--to-sequence)
                          (lsp--locations-to-xref-items)
                          (-filter 'identity))))
          (unless xrefs
            (user-error "No inheritances found for: %s" input))
          (+amos-ivy-xref xrefs 'inheritances))))

  (defun ccls/base () (interactive) (+amos-lsp-find-custom 'base "$ccls/base"))
  (defun ccls/callers () (interactive) (+amos-lsp-find-custom 'callers "$ccls/callers"))
  (defun ccls/vars (kind) (+amos-lsp-find-custom 'vars "$ccls/vars" (plist-put (lsp--text-document-position-params) :kind kind)))
  (defun ccls/random () (interactive) (+amos-lsp-find-custom 'random "$ccls/random"))

  ;; References w/ Role::Address bit (e.g. variables explicitly being taken addresses)
  (defun ccls/references-address ()
    (interactive)
    (+amos-lsp-find-custom
     'address "textDocument/references"
     (plist-put (lsp--text-document-position-params) :context
                '(:role 128))))

  ;; References w/ Role::Dynamic bit (macro expansions)
  (defun ccls/references-macro ()
    (interactive)
    (+amos-lsp-find-custom
     'address "textDocument/references"
     (plist-put (lsp--text-document-position-params) :context
                '(:role 64))))

  ;; References w/o Role::Call bit (e.g. where functions are taken addresses)
  (defun ccls/references-not-call ()
    (interactive)
    (+amos-lsp-find-custom
     'address "textDocument/references"
     (plist-put (lsp--text-document-position-params) :context
                '(:excludeRole 32))))

  ;; References w/ Role::Read
  (defun ccls/references-read ()
    (interactive)
    (+amos-lsp-find-custom
     'read "textDocument/references"
     (plist-put (lsp--text-document-position-params) :context
                '(:role 8))))

  ;; References w/ Role::Write
  (defun ccls/references-write ()
    (interactive)
    (+amos-lsp-find-custom
     'write "textDocument/references"
     (plist-put (lsp--text-document-position-params) :context
                '(:role 16))))

  (defalias 'lsp-cquery-enable 'lsp-ccls-enable)
  (defalias 'cquery-call-hierarchy           'ccls-call-hierarchy)
  (defalias 'cquery/callers                  'ccls/callers)
  (defalias 'cquery-member-hierarchy         'ccls-member-hierarchy)
  (defalias 'cquery-inheritance-hierarchy    'ccls-inheritance-hierarchy)
  (add-hook 'c-mode-common-hook #'ccls//enable)
  (add-hook 'doom-escape-hook #'ccls/diagnostic)
  (add-hook 'lsp-after-diagnostics-hook #'flycheck-buffer))

(defun ccls//enable ()
  (direnv-update-environment)
  (lsp-ccls-enable)
  (setq-local flycheck-checker 'lsp-ui)
  (lsp-ui-flycheck-add-mode major-mode)
  (add-to-list 'flycheck-checkers 'lsp-ui)
  (setq lsp-ui-flycheck-live-reporting nil)
  (dolist (c '(c/c++-clang c/c++-gcc c/c++-cppcheck))
    (setq flycheck-checkers (delq c flycheck-checkers))))

(set-lookup-handlers! '(c-mode c++-mode)
  :definition #'xref-find-definitions
  :references #'xref-find-references
  :documentation #'counsel-dash-at-point)


(defun +amos*lsp--position-to-point (params)
  "Convert Position object in PARAMS to a point."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char 1)
      ;; The next line calculs the point from the LSP position.
      ;; We use `goto-char' to ensure that we return a point inside the buffer
      ;; to avoid out of range error
      (goto-char (+ (line-beginning-position (1+ (gethash "line" params)))
                    (gethash "character" params)))
      (point))))

(advice-add #'lsp--position-to-point :override #'+amos*lsp--position-to-point)
