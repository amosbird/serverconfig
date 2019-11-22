;;; lang/cc/config.el --- c, c++, and obj-c -*- lexical-binding: t; -*-

(defvar +cc-default-header-file-mode 'c-mode
  "Fallback major mode for .h files if all other heuristics fail (in
`+cc-c-c++-objc-mode').")

(use-package! cc-mode
  :mode ("\\.mm\\'" . objc-mode)
  :init
  (setq-default c-basic-offset tab-width
                c-backspace-function #'delete-backward-char
                c-default-style "amos")

  ;; The plusses in c++-mode can be annoying to search for ivy/helm (which reads
  ;; queries as regexps), so we add these for convenience.
  (defalias 'cpp-mode 'c++-mode)
  (defvaralias 'cpp-mode-map 'c++-mode-map)

  ;; Activate `c-mode', `c++-mode' or `objc-mode' depending on heuristics
  (add-to-list 'auto-mode-alist '("\\.h\\'" . +cc-c-c++-objc-mode))

  :config
  (set-electric! '(c-mode c++-mode objc-mode java-mode) :chars '(?\n ?\} ?\{))

  (set-lookup-handlers! '(c-mode c++-mode)
    :definition #'+amos/definitions
    :references #'+amos/references
    :documentation #'counsel-dash-at-point)

  (set-rotate-patterns! 'c++-mode
    :symbols '(("public" "protected" "private")
               ("class" "struct")))

  (set-company-backend!
    '(c-mode c++-mode objc-mode)
    'company-lsp)

  (set-pretty-symbols! '(c-mode c++-mode)
                       ;; Functional
                       ;; :def "void "
                       ;; Types
                       :null "nullptr"
                       :true "true" :false "false"
                       :int "int" :float "float"
                       :str "std::string"
                       :bool "bool"
                       ;; Flow
                       :not "!"
                       :and "&&" :or "||"
                       :for "for"
                       :return "return"
                       :yield "#require")

  ;;; Better fontification (also see `modern-cpp-font-lock')
  (add-hook! 'c-mode-common-hook
    (modify-syntax-entry ?_ "w")
    (rainbow-delimiters-mode +1))
  (add-hook! (c-mode c++-mode) #'+cc|fontify-constants)
  (setq-default c-noise-macro-names '("constexpr"))


  ;;; Keybindings
  (map! (:map (c-mode-map c++-mode-map)
          "<" nil
          ">" nil
          :i "RET"      #'+cc-append-comment-line
          :n "RET"      #'+amos/lsp-highlight-symbol
          :i ";"        #'+amos/better-semicolon
          :n "C-e"      #'+amos/maybe-add-end-of-statement
          :n "M-v"      #'+amos/lsp-ui-imenu
          :n "gs"       #'ccls/workspace-symbol
          :n "gS"       (lambda! (setq current-prefix-arg t) (call-interactively #'ccls/workspace-symbol))
          :n "gh"       #'ccls-call-hierarchy
          :n "gR"       #'ccls/callers
          :n "gb"       #'ccls/inheritances
          :n "gt"       (lambda! (ccls/inheritance t))
          :n "gT"       #'ccls/inheritance
          :n "ge"       #'lsp-execute-code-action
          :n "M-u"      #'ccls-code-lens-mode
          :n "M-o"      #'lsp-ui-sideline-mode
          "C-c i"       #'ccls/includes
          "C-c I"       (lambda! (ccls/includes t))))
  (sp-with-modes '(c++-mode objc-mode)
    (sp-local-pair "<" ">"
                   :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p)
                   :post-handlers '(("| " "SPC"))))

  (sp-with-modes '(c-mode c++-mode objc-mode java-mode)
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC")))))

(use-package! cmake-mode
  :defer
  :mode
  (("/CMakeLists\\.txt\\'" . cmake-mode)
   ("\\.cmake\\'" . cmake-mode)))

(use-package! disaster
  :defer
  :commands disaster)

(use-package! cuda-mode
  :defer
  :mode "\\.cuh?$")

(use-package! opencl-mode
  :defer
  :mode "\\.cl$")

(use-package! demangle-mode
  :init (add-hook! 'llvm-mode-hook #'demangle-mode)
  :defer)

(use-package! modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode)
  :defer)

(setq ccls-enabled nil)
(defun +amos-diagnostic-maybe-h ()
  (interactive)
  (when ccls-enabled
    (ccls/diagnostic))
  nil)
(add-hook! 'doom-escape-hook #'+amos-diagnostic-maybe-h)

(defun +amos-ccls-enable-h ()
  (interactive)
  (require 'ccls)
  (let ((name (buffer-name)))
    (unless (or (string-prefix-p "timemachine:" name)
                (string-suffix-p "~" name))
      (require 'lsp-mode)
      (flycheck-mode +1)
      ;; (eldoc-mode -1)
      (when (--any? (s-starts-with? it default-directory) +amos-system-header-paths)
        (c-set-style "gnu"))
      (add-hook 'lsp-after-diagnostics-hook #'flycheck-buffer nil t)
      (direnv-update-environment)
      (condition-case nil
          (doom-with-advice (lsp--info #'ignore) (lsp))
        (user-error nil))
      (setq-local ccls-enabled t)
      (setq-local flycheck-checker 'lsp-ui)
      (lsp-ui-flycheck-add-mode major-mode)
      (add-to-list 'flycheck-checkers 'lsp-ui)
      (setq-local lsp-ui-flycheck-live-reporting nil)
      (dolist (c '(c/c++-clang c/c++-gcc c/c++-cppcheck))
        (setq flycheck-checkers (delq c flycheck-checkers))))))

(add-hook! (c-mode c++-mode) #'+amos-ccls-enable-h)
(add-hook! (c-mode c++-mode) (electric-indent-local-mode -1))

(use-package! ccls
  ;; :commands (+amos-ccls-enable-h)  ; autoload fails
  :defer
  :init
  (setq
   ;; ccls-sem-highlight-method 'font-lock
   ;; ccls-sem-highlight-method 'overlay
   ccls-sem-highlight-method nil
   )

  :config
  (defun ccls/workspace-symbol (pattern)
    (interactive (list (read-string
                        "workspace/symbol: "
                        nil 'xref--read-pattern-history)))
    (let ((symbols (lsp-request
                    "workspace/symbol"
                    `(:query ,pattern :folders ,(if current-prefix-arg (vector (doom-project-root)) (vector default-directory))))))
      (unless symbols
        (user-error "No symbol found for: %s" pattern))
      (+amos-ivy-xref
       (mapcar (lambda (x) (lsp--symbol-information-to-xref x)) symbols) pattern)))
  (defun ccls/includes (&optional force)
    (interactive)
    (let ((x (intern (concat (doom-project-root) "--includes"))))
      (unless (and (boundp x) (not force))
        (setq x (lsp-request "$ccls/includes" nil)))
      (ivy-read "Include: " x :action #'+amos/add-include)))
  (defun ccls/fileinfo ()
    (interactive)
    (let ((hashmap (lsp-request
                    "$ccls/fileInfo"
                    (list :textDocument (lsp--text-document-identifier)))))
      (with-current-buffer (generate-new-buffer "*temp*")
        (insert (format "path = %s\n args = %s"
                        (gethash "path" hashmap)
                        (gethash "args" hashmap)))
        (+popup/buffer))))
  (defun ccls/diagnostic ()
    (interactive)
    (lsp-notify
     "$ccls/diagnostic"
     (list :textDocument (lsp--text-document-identifier))))
  (defun ccls/inheritances ()
    (interactive)
    (+amos-lsp-find-custom 'inheritances "$ccls/inheritance" `(:levels 1000 :derived t)))
  (defun ccls/callee ()
    (interactive)
    (+amos-lsp-find-custom 'callee "$ccls/call" '(:callee t)))
  (defun ccls/caller ()
    (interactive)
    (+amos-lsp-find-custom 'caller "$ccls/call"))
  (defun ccls/vars (kind)
    (+amos-lsp-find-custom 'vars "$ccls/vars" `(:kind ,kind)))
  (defun ccls/base (levels)
    (+amos-lsp-find-custom 'base "$ccls/inheritance" `(:levels ,levels)))
  (defun ccls/derived (levels)
    (+amos-lsp-find-custom 'derived "$ccls/inheritance" `(:levels ,levels :derived t)))
  (defun ccls/member (kind)
    (+amos-lsp-find-custom 'member "$ccls/member" `(:kind ,kind)))
  (defun ccls/member-function ()
    (interactive)
    (ccls/member 3))
  (defun ccls/member-type ()
    (interactive)
    (ccls/member 2))
  (defun ccls/member-field ()
    (interactive)
    (ccls/member 1))

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
                '(:role 16)))))

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

(setq +amos-system-header-paths
      '("/usr/local/include"
        "/usr/include"
        ))

(c-add-style "amos"
             '("cc-mode"
               (fill-column . 100)
               (indent-tabs-mode . nil)
               (c-offsets-alist . ((innamespace . 0)
                                   (arglist-intro . ++)
                                   (arglist-cont-nonempty . ++)
                                   (substatement-open . 0)
                                   (inlambda . 0)
                                   (inline-open . 0)
                                   (member-init-intro . ++)
                                   (statement-cont . +cc-llvm-lineup-statement)))))


;; https://github.com/Fuco1/smartparens/issues/963
(after! smartparens
  (push 'c-electric-brace sp--special-self-insert-commands)
  (push 'c-electric-paren sp--special-self-insert-commands))

(defun +amos|iedit-mode-hook (&rest _)
  (when (memq major-mode '(c-mode c++-mode))
    (advice-add #'lsp-on-change :override #'ignore)
    (advice-add #'lsp-before-change :override #'ignore)
    (advice-add #'jit-lock-after-change :override #'ignore)
    (advice-add #'flycheck-handle-change :override #'ignore)
    (advice-add #'c-font-lock-fontify-region :override #'ignore)
    (advice-add #'c-called-from-text-property-change-p :override #'+amos*yes)
    (remove-hook 'before-change-functions #'+amos|iedit-mode-hook t)
    )
  t)
(defun +amos|iedit-mode-hook-after (&rest _)
  (when (memq major-mode '(c-mode c++-mode))
    (doom-modeline-update-buffer-file-state-icon)
    (doom-modeline-update-buffer-file-name)
    (advice-add #'doom-modeline-update-buffer-file-name :override #'ignore)
    (advice-add #'doom-modeline-update-buffer-file-state-icon :override #'ignore)
    (remove-hook 'after-change-functions #'+amos|iedit-mode-hook-after t)
    )
  t)
(defun +amos|iedit-setup-hooks ()
  (add-hook 'before-change-functions #'+amos|iedit-mode-hook nil t)
  (add-hook 'after-change-functions #'+amos|iedit-mode-hook-after nil t))

(defun +amos|iedit-mode-end-hook ()
  (when (memq major-mode '(c-mode c++-mode))
    (lsp-on-revert)
    (advice-remove #'lsp-on-change #'ignore)
    (advice-remove #'lsp-before-change #'ignore)
    (advice-remove #'doom-modeline-update-buffer-file-name #'ignore)
    (advice-remove #'doom-modeline-update-buffer-file-state-icon #'ignore)
    (advice-remove #'jit-lock-after-change #'ignore)
    (advice-remove #'flycheck-handle-change #'ignore)
    (advice-remove #'c-font-lock-fontify-region #'ignore)
    (advice-remove #'c-called-from-text-property-change-p #'+amos*yes)
    (doom-modeline-update-buffer-file-state-icon)
    (doom-modeline-update-buffer-file-name)
    )
  (font-lock-mode +1)
  t)
;; (add-hook 'iedit-mode-hook #'+amos|iedit-setup-hooks)
;; (add-hook 'iedit-mode-end-hook #'+amos|iedit-mode-end-hook)

;; maybe useful?
;; (add-hook! (c-mode c++-mode) (setq iedit-auto-bufferring t))
