;;; lang/cc/config.el --- c, c++, and obj-c -*- lexical-binding: t; -*-

(defvar +cc-default-header-file-mode 'c-mode
  "Fallback major mode for .h files if all other heuristics fail (in
`+cc-c-c++-objc-mode').")

(defun +amos/format-region-or-buffer ()
  "Format the selected region, or whole buffer if nothing is selected."
  (interactive)
  (call-interactively
   (if (doom-region-active-p)
       #'clang-format-region
     #'clang-format-buffer)))

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
  (set-electric! '(c-mode c++-mode c-ts-base-mode objc-mode java-mode) :chars '(?\n ?\} ?\{))

  (set-rotate-patterns! 'c++-mode
    :symbols '(("public" "protected" "private")
               ("class" "struct")))

  (set-ligatures! '(c-mode c++-mode c-ts-base-mode)
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
  (add-hook! (c-mode c++-mode) #'+cc|fontify-constants)
  (setq-default c-noise-macro-names '("constexpr"))

    ;;; Keybindings
  (map! (:map (c-mode-map c++-mode-map c-ts-base-mode-map)
         "<" nil
         ">" nil
         :i ";"        #'+amos/better-semicolon
         :n "C-e"      #'+amos/maybe-add-end-of-statement
         :n "M-v"      #'+amos/lsp-ui-imenu
         :nv "S-<f11>" #'+amos/format-region-or-buffer ;; terminal
         :n "ge"       #'lsp-execute-code-action
         :n "M-o"      #'lsp-ui-sideline-mode))
  (sp-with-modes '(c++-mode objc-mode c-ts-base-mode)
    (sp-local-pair "<" ">"
                   :when '(+cc-sp-point-is-template-p +cc-sp-point-after-include-p)
                   :post-handlers '(("| " "SPC"))))

  (sp-with-modes '(c-mode c++-mode c-ts-base-mode objc-mode java-mode)
    (sp-local-pair "<" ">" :actions :rem)
    (sp-local-pair "{" nil :post-handlers '(:add +amos-cc-brace-indent))
    (sp-local-pair "/*!" "*/" :post-handlers '(("||\n[i]" "RET") ("[d-1]< | " "SPC")))))


(defun +amos-cc-brace-indent (id action _)
  (when (and (eq action 'insert))
    (save-excursion
      (if (string-match "^[[:blank:]]*$" (buffer-substring (line-beginning-position) (- (point) (length id))))
          (indent-according-to-mode)))))

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

(add-hook! (c-mode c++-mode c-ts-base-mode) #'lsp-deferred)
;; (add-hook! (c-mode c++-mode c-ts-base-mode) #'eglot-ensure)
(after! lsp-clangd (set-lsp-priority! 'clangd 2))
(setq lsp-clients-clangd-args `(
                                "--log=error"
                                "--malloc-trim"
                                "--background-index"
                                "--clang-tidy"
                                "--completion-style=detailed"
                                "--pch-storage=memory"
                                "--header-insertion=never"
                                "--header-insertion-decorators=0"))

(after! eglot
  (setq eglot-autoshutdown t)
  (setq eglot-extend-to-xref t)
  (setq eglot-send-changes-idle-time 0.2)
  (setq eglot-ignored-server-capabilities
        '(:documentFormattingProvider
          :documentRangeFormattingProvider
          :documentOnTypeFormattingProvider
          :documentLinkProvider
          :foldingRangeProvider
          :signatureHelpProvider
          :hoverProvider))

  (add-to-list 'eglot-server-programs
               '((c-mode c++-mode)
                 . ("clangd"
                    "--log=error"
                    "--malloc-trim"
                    "--background-index"
                    "--clang-tidy"
                    "--completion-style=detailed"
                    "--pch-storage=memory"
                    "--header-insertion=never"
                    "--header-insertion-decorators=0")))

  (defun +amos/eglot-clangd-find-other-file (&optional new-window)
    "Switch between the corresponding C/C++ source and header file.
If NEW-WINDOW (interactively the prefix argument) is non-nil,
open in a new window.

Only works with clangd."
    (interactive "P")
    (let* ((res
            (jsonrpc-request (eglot--current-server-or-lose)
                             :textDocument/switchSourceHeader
                             (eglot--TextDocumentIdentifier)))
           (other (eglot--uri-to-path res)))
      (if (string= other "")
          (message "no other file found")
        (funcall (if new-window #'find-file-other-window #'find-file) other))))

  (defun eglot--remove-overlays (name)
    (save-restriction
      (widen)
      (remove-overlays (point-min) (point-max) name t)))

  ;; TODO it doesn't work
  (defun eglot--make-links (links)
    (mapc
     (lambda (link)
       (cl-destructuring-bind
           (:range (:end end :start start) :target target)
           link
         (-doto (make-button (eglot--lsp-position-to-point start)
                             (eglot--lsp-position-to-point end))
           (overlay-put 'lsp-link t))
         ))
     links))

  (defun +amos/eglot-document-links ()
    (interactive)
    (jsonrpc-async-request (eglot--current-server-or-lose)
                           :textDocument/documentLink `(:textDocument ,(eglot--TextDocumentIdentifier))
                           :success-fn #'eglot--make-links
                           :deferred :textDocument/documentLink))
  )


(defun +amos|remap-cpp-faces ()
  (add-function :before-until (local 'tree-sitter-hl-face-mapping-function)
                (lambda (capture-name)
                  (pcase capture-name
                    ("type.builtin" 'font-lock-type-face)
                    ("property" 'font-lock-constant-face)
                    )))
  )
(add-hook! (c-mode c++-mode) #'+amos|remap-cpp-faces)
;; (add-hook! (c-mode c++-mode) #'tree-sitter-mode)
;; (after! tree-sitter
  ;; (global-tree-sitter-mode 1)
  ;; )
;; (tree-sitter-hl-add-patterns 'cpp
;;   [
;;    (qualified_identifier left: (namespace_identifier) @type.builtin right: (_))
;;    ])

;; (add-hook! (c-mode c++-mode) #'tree-sitter-mode)

;; (defface tree-sitter-hl-face:function
;; (defface tree-sitter-hl-face:function.call
;; (defface tree-sitter-hl-face:function.builtin
;; (defface tree-sitter-hl-face:function.special
;; (defface tree-sitter-hl-face:function.macro
;; (defface tree-sitter-hl-face:method
;; (defface tree-sitter-hl-face:method.call
;; (defface tree-sitter-hl-face:type
;; (defface tree-sitter-hl-face:type.parameter
;; (defface tree-sitter-hl-face:type.argument
;; (defface tree-sitter-hl-face:type.builtin
;; (defface tree-sitter-hl-face:type.super
;; (defface tree-sitter-hl-face:constructor
;; (defface tree-sitter-hl-face:variable
;; (defface tree-sitter-hl-face:variable.parameter
;; (defface tree-sitter-hl-face:variable.builtin
;; (defface tree-sitter-hl-face:variable.special
;; (defface tree-sitter-hl-face:property
;; (defface tree-sitter-hl-face:property.definition
;; (defface tree-sitter-hl-face:comment
;; (defface tree-sitter-hl-face:doc
;; (defface tree-sitter-hl-face:string
;; (defface tree-sitter-hl-face:string.special
;; (defface tree-sitter-hl-face:escape
;; (defface tree-sitter-hl-face:embedded
;; (defface tree-sitter-hl-face:keyword
;; (defface tree-sitter-hl-face:operator
;; (defface tree-sitter-hl-face:label
;; (defface tree-sitter-hl-face:constant
;; (defface tree-sitter-hl-face:constant.builtin
;; (defface tree-sitter-hl-face:number
;; (defface tree-sitter-hl-face:punctuation
;; (defface tree-sitter-hl-face:punctuation.bracket
;; (defface tree-sitter-hl-face:punctuation.delimiter
;; (defface tree-sitter-hl-face:punctuation.special
;; (defface tree-sitter-hl-face:tag
;; (defface tree-sitter-hl-face:attribute

(add-hook! (c-ts-base-mode) (electric-indent-local-mode +1) (setq fill-column 120))

(defun +amos/add-include (h &rest others)
  "Add an #include line for `h' near top of file, avoiding duplicates."
  (interactive "M#include: ")
  (dolist (header (cons h others))
    (let ((incl (format "#include <%s>" header)))
      (save-excursion
        (if (search-backward incl nil t)
            nil
          (when (re-search-backward "\\(#include\\|#pragma once\\)" nil t)
            (if (looking-at "#pragma once")
                (forward-line))
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
               (fill-column . 120)
               (indent-tabs-mode . nil)
               (c-offsets-alist . ((innamespace . 0)
                                   (arglist-intro . ++)
                                   (arglist-cont-nonempty . ++)
                                   (substatement-open . 0)
                                   (inlambda . 0)
                                   (inline-open . 0)
                                   (member-init-intro . +)
                                   (statement-cont . +cc-llvm-lineup-statement)))))

(defun +amos|iedit-mode-hook (&rest _)
  (when (memq major-mode '(c-mode c++-mode c-ts-base-mode))
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
  (when (memq major-mode '(c-mode c++-mode c-ts-base-mode))
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
  (when (memq major-mode '(c-mode c++-mode c-ts-base-mode))
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

(after! treesit
  (cl-pushnew
   (cons 'top-level-namespace
         (lambda ()
           (lambda (node parent &rest _)
             (when-let* ((gp (treesit-node-parent parent))
                         (ggp (treesit-node-parent gp)))
               (and
                (string-match-p "namespace_definition" (treesit-node-type gp))
                (string-match-p "translation_unit" (treesit-node-type ggp)))))))
   treesit-simple-indent-presets))

(defun +amos-indent-style()
  "Override the built-in GNU indentation style with some additional rules."
  `(
    ((top-level-namespace) parent-bol 0)
    ((query "(for_statement body: (compound_statement \"{\") @indent)") parent-bol 0)
    ((query "(for_range_loop (compound_statement \"{\") @indent)") parent-bol 0)
    ((query "(if_statement consequence: (compound_statement \"{\") @indent)") parent-bol 0)
    ((query "(else_clause (compound_statement \"{\") @indent)") parent-bol 0)
    ((query "(else_clause (_) @indent)") parent-bol 4)
    ((parent-is "else_statement") parent-bol 4)
    ((query "(while_statement body: (compound_statement \"{\") @indent)") parent-bol 0)

    ;; TODO switch, do, case

    ((query "(switch_statement body: (_) @indent)") parent-bol 0)

    ((query "(case_statement (compound_statement _) @indent)") parent-bol 0)
    ((query "(do_statement body: (_) @indent)") parent-bol 0)

    ((parent-is "switch_statement") standalone-parent c-ts-mode-indent-offset)
    ,@(alist-get 'cpp treesit-simple-indent-rules)
    ))


;; TODO: Figure out how to extend this properly
(defun +amos*c-ts-mode--simple-indent-rules (mode style)
  "Return the indent rules for MODE and STYLE.

The returned value can be set to `treesit-simple-indent-rules'.
MODE can be `c' or `cpp'.  STYLE can be `gnu', `k&r', `linux', `bsd'."
  (let ((rules
         `((c-ts-mode--for-each-tail-body-matcher
            prev-line c-ts-mode-indent-offset)

           ((top-level-namespace) parent-bol 0)
           ((query "(for_statement body: (compound_statement \"{\") @indent)") parent-bol 0)
           ((query "(if_statement consequence: (compound_statement \"{\") @indent)") parent-bol 0)
           ((query "(else_clause (compound_statement \"{\") @indent)") parent-bol 0)
           ((query "(else_clause (_) @indent)") parent-bol 4)
           ((parent-is "else_statement") parent-bol 4)
           ((query "(while_statement body: (compound_statement \"{\") @indent)") parent-bol 0)

           ;; TODO switch, do, case

           ((query "(switch_statement body: (_) @indent)") parent-bol 0)

           ((query "(case_statement (compound_statement _) @indent)") parent-bol 0)
           ((query "(do_statement body: (_) @indent)") parent-bol 0)

           ((parent-is "switch_statement") standalone-parent c-ts-mode-indent-offset)

           ;; Misc overrides.
           ((parent-is "translation_unit") column-0 0)
           ((node-is ,(rx (or "else" "case"))) standalone-parent 0)
           ;; Align the while keyword to the do keyword.
           ((match "while" "do_statement") parent 0)
           c-ts-mode--parenthesized-expression-indent-rule
           ;; Thanks to tree-sitter-c's weird for-loop grammar, we can't
           ;; use the baseline indent rule for it.
           c-ts-mode--for-loop-indent-rule
           c-ts-mode--label-indent-rules
           ,@c-ts-mode--preproc-indent-rules
           c-ts-mode--macro-heuristic-rules

           ;; Make sure type and function definition components align and
           ;; don't indent. Also takes care of GNU style opening braces.
           ((parent-is ,(rx (or "function_definition"
                                "struct_specifier"
                                "enum_specifier"
                                "function_declarator"
                                "template_declaration")))
            parent 0)
           ;; This is for the trailing-star stype:  int *
           ;;                                       func()
           ((match "function_declarator" nil "declarator") parent-bol 0)
           ;; ((match nil "function_definition" "declarator") parent 0)
           ;; ((match nil "struct_specifier" "name") parent 0)
           ;; ((match nil "function_declarator" "parameters") parent 0)
           ;; ((parent-is "template_declaration") parent 0)

           ;; `c-ts-common-looking-at-star' has to come before
           ;; `c-ts-common-comment-2nd-line-matcher'.
           ;; FIXME: consolidate into a single rule.
           ((and (parent-is "comment") c-ts-common-looking-at-star)
            c-ts-common-comment-start-after-first-star -1)
           (c-ts-common-comment-2nd-line-matcher
            c-ts-common-comment-2nd-line-anchor
            1)
           ((parent-is "comment") prev-adaptive-prefix 0)

           ;; Preproc directives
           ((node-is "preproc_arg") no-indent)
           ((node-is "preproc") column-0 0)
           ((node-is "#endif") column-0 0)

           ;; C++
           ((node-is "access_specifier") parent-bol 0)
           ((prev-line-is "access_specifier")
            parent-bol c-ts-mode-indent-offset)

           c-ts-common-baseline-indent-rule)))
    (setq rules
          (pcase style
            ('gnu rules)
            ('k&r rules)
            ('linux
             ;; Reference:
             ;; https://www.kernel.org/doc/html/latest/process/coding-style.html,
             ;; and script/Lindent in Linux kernel repository.
             `(((node-is "labeled_statement") column-0 0)
               ,@rules))
            ('bsd
             `(((match "compound_statement" "compound_statement")
                standalone-parent c-ts-mode-indent-offset)
               ((node-is "compound_statement") standalone-parent 0)
               ,@rules))))
    (pcase mode
      ('c `((c . ,rules)))
      ('cpp `((cpp .
               (((query "(for_range_loop (compound_statement \"{\") @indent)") parent-bol 0)
                ,@rules)
               ))))))

(advice-add #'c-ts-mode--simple-indent-rules :override #'+amos*c-ts-mode--simple-indent-rules)

(setq c-ts-mode-indent-offset 4
      ;; c-ts-mode-indent-style #'+amos-indent-style
      treesit-font-lock-level 4)

(add-hook! (c-mode-common c-ts-base-mode)
  (modify-syntax-entry ?_ "w")
  ;; (rainbow-delimiters-mode +1)

  (treesit-font-lock-recompute-features)

  (when-let ((p (getenv "envprompt")))
    (if (string= p "CC-Playground")
        (cc-playground-mode +1))))
