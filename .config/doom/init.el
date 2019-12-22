;;;  -*- lexical-binding: t; no-byte-compile: t; -*-

(doom! :completion
       company
       ;; (helm +fuzzy)
       (ivy +fuzzy)

       :ui
       doom
       doom-quit
       modeline
       hl-todo
       nav-flash
       unicode ;; for mu4e
       popup

       :editor
       (evil +everywhere)
       file-templates
       format
       rotate-text
       multiple-cursors
       snippets
       ;; syntax-checker

       :emacs
       ;; dired
       electric
       vc

       :tools
       eval
       make
       magit
       (lookup)
       flyspell

       :lang
       assembly
       data
       emacs-lisp
       (latex +latexmk +zathura)
       lua
       markdown
       (sh +fish)
       ;; rust
       (ocaml +lsp)
       (org +pandoc)
       ;; (python +lsp)
       python
       (go +lsp)

       :private
       amos-cc
       ;; amos-java
       amos-email
       ;; amos-go
       amos-modeline
       amos-org
       leap
       ;; amos-python
       ;; amos-sh
       )

;; (setq TeX-command "pdftex")
;; (setq LaTeX-command "pdflatex")
;; (setq-default TeX-engine 'xetex)
(setq so-long-target-modes '(css-mode sgml-mode nxml-mode))
(setq load-prefer-newer t)
(setq shr-width 100)
(setq all-the-icons-scale-factor 1.0)
(setq cmake-tab-width 4)
(setq doom-font (font-spec :family "Ubuntu Mono" :size 18))
(setq doom-modeline-icon nil)
(setq display-line-numbers-type 'relative)
;; fix start-process xdg-open
(setq process-connection-type nil)
;; (setq auto-revert-interval 0.3)
;; (setq auto-revert-verbose nil)
(setq auto-save-visited-interval 5)
(setq auto-save-visited-mode nil)
(setq avy-timeout-seconds 0.2)
(setq better-jumper-use-evil-jump-advice nil)
(setq bibtex-completion-browser-function 'browser-url-chromium)
(setq bibtex-completion-pdf-open-function (lambda (fpath) (call-process "zathura" nil 0 nil fpath)))
(setq browse-url-chrome-program (expand-file-name "~/scripts/vivaldi"))
(setq browse-url-firefox-program "luakit")
(setq browse-url-mailto-function 'mu4e~compose-browse-url-mail)
(setq comint-move-point-for-output 'this)
(setq company-auto-complete nil)
(setq company-idle-delay 0)
(setq company-dabbrev-ignore-case t)
(setq company-dabbrev-code-ignore-case t)
(setq company-dabbrev-code-everywhere t)
(setq company-dabbrev-other-buffers t)
(setq company-dabbrev-ignore-buffers (lambda (buffer) (not (projectile-project-buffer-p buffer (projectile-project-root)))))
(setq company-show-numbers t)
(setq company-transformers nil)
(setq company-lsp-cache-candidates nil)
(setq counsel-rg-base-command "rg -S --no-heading --line-number --color never -- %s .")
(setq dired-create-destination-dirs 'always)
(setq dired-omit-verbose nil)
(setq dired-open-functions '(dired-open-by-extension dired-open-call-function-by-extension dired-open-subdir))
(setq dired-open-extensions-elisp
      '(("el.gz" . find-file)
        ("zip" . +amos/compress-view)
        ("jar" . +amos/compress-view)
        ("tgz" . +amos/compress-view)
        ("apk" . +amos/compress-view)
        ("deb" . +amos/compress-view)
        ("rpm" . +amos/compress-view)
        ("gz" . +amos/compress-view)
        ("rar" . +amos/compress-view)
        ("tar" . +amos/compress-view)
        ("xz" . +amos/compress-view)
        ("lz" . +amos/compress-view)
        ("lzh" . +amos/compress-view)
        ("lzma" . +amos/compress-view)
        ("bz2" . +amos/compress-view)
        ("7z" . +amos/compress-view)))
(setq dired-open-extensions
      '(("pdf" . "xdg-open")
        ("mp3" . "xdg-open")
        ("mp4" . "xdg-open")
        ("mpg" . "xdg-open")
        ("webm" . "xdg-open")
        ("flac" . "xdg-open")
        ("m4a" . "xdg-open")
        ("aac" . "xdg-open")
        ("mp4" . "xdg-open")
        ("mov" . "xdg-open")
        ("ps" . "xdg-open")
        ("gif" . "xdg-open")
        ("svg" . "xdg-open")
        ("jpg" . "xdg-open")
        ("jpeg" . "xdg-open")
        ("png" . "xdg-open")
        ("jfif" . "xdg-open")
        ("iso" . "xdg-open")
        ("callgrind.out.*" . "kcachegrind")
        ("doc" . "xdg-open")
        ("docx" . "xdg-open")
        ;; ("html" . "xdg-open")
        ("xls" . "xdg-open")
        ("xlsx" . "xdg-open")
        ("odt" . "xdg-open")
        ("ppt" . "xdg-open")
        ("pptx" . "xdg-open")
        ("mkv" . "xdg-open")
        ("torrent" . "xdg-open")))
(setq dired-open-find-file-function #'+amos/find-file)
(setq dired-recursive-copies 'always)
(setq dired-recursive-deletes 'always)
(setq dired-auto-revert-buffer t)
(setq global-auto-revert-non-file-buffers t)
(setq eldoc-message-function (lambda (&rest args) (let ((message-truncate-lines t)) (apply #'eldoc-minibuffer-message args))))
(setq evil-cjk-emacs-word-boundary t)
(setq evil-esc-delay 0.001)
(setq evil-ex-substitute-global t)
(setq evil-args-openers '("(" "{" "[" "<"))
(setq evil-args-closers '(")" "}" "]" ">"))
(setq evil-kill-on-visual-paste nil)
(setq evil-mc-cursor-overlay-priority 10000)
(setq evil-shift-round nil)
(setq evil-snipe-auto-scroll nil)
(setq evil-snipe-scope 'visible)
(setq evil-want-C-i-jump t)
(setq easy-hugo-basedir "~/git/blog/")
(setq easy-hugo-url "https://wentropy.com")
(setq easy-hugo-server-flags "-D")
(setq easy-hugo-sshdomain "wentropy.com")
(setq easy-hugo-root "/var/www/blog/")
(setq easy-hugo-previewtime "300")
(setq easy-hugo-default-ext ".org")
(setq explicit-shell-file-name "/bin/bash")
(setq find-file-visit-truename t)
(setq inhibit-compacting-font-caches nil)
(setq flycheck-disabled-checkers '(c/c++-clang c/c++-gcc c/c++-cppcheck))
(setq fringes-outside-margins t)
;; (setq global-auto-revert-non-file-buffers t)
(setq indent-tabs-mode t)
(setq initial-buffer-choice t)
(setq interprogram-paste-function nil)
(setq ivy-do-completion-in-region nil)
(setq ivy-fixed-height-minibuffer t)
(setq ivy-format-function 'ivy-format-function-line)
(setq ivy-height 12)
(setq ivy-magic-slash-non-match-action nil)
(setq ivy-mode t)
(setq ivy-rich-switch-buffer-align-virtual-buffer t)
(setq ivy-rich-switch-buffer-delimiter "|")
(setq ivy-use-selectable-prompt t)
(setq ivy-use-virtual-buffers nil)
(setq ivy-virtual-abbreviate 'full)
(setq google-translate-translation-directions-alist '(("en" . "zh-CN")))
(setq lsp-response-timeout 5)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-ui-doc-enable nil)
(setq lsp-signature-render-all nil)
(setq lsp-enable-on-type-formatting nil)
(setq message-truncate-lines t)
(setq nav-flash-delay 0.3)
(setq org-M-RET-may-split-line '((default)))
(setq org-agenda-files '("~/org/todo.org"))
(setq org-src-tab-acts-natively nil)
(setq org-babel-load-languages
      '((python . t)
        (emacs-lisp . t)
        (dot . t)
        (gnuplot . t)
        (C . t)
        (sql . t)
        (awk . t)))
(setq org-beamer-frame-level 2)
(setq org-beamer-theme "metropolis")
(setq org-capture-templates
      '(("c" "code" entry
         (file+headline "~/org/code.org" "Triage")
         "** %a " :prepend t :empty-lines-before 1 :empty-lines-after 1)
        ("i" "idea" entry
         (file "~/org/idea.org")
         "* %u %?\n%i" :prepend t :empty-lines-before 1 :empty-lines-after 1)
        ("n" "notes" entry
         (file "~/org/notes.org")
         (file "~/org/template/idea")
         :empty-lines-before 1 :empty-lines-after 1)
        ("t" "Templates for todo items")
        ("te" "ergonomics" entry
         (file+headline "~/org/todo.org" "Ergonomics")
         "** TODO %?" :prepend t :empty-lines-before 1 :empty-lines-after 1)
        ("tw" "working" entry
         (file+headline "~/org/todo.org" "Work")
         "** TODO %?" :prepend t :empty-lines-before 1 :empty-lines-after 1)
        ("tl" "learning" entry
         (file+headline "~/org/todo.org" "Learning")
         "** TODO %?" :prepend t :empty-lines-before 1 :empty-lines-after 1)))
(setq org-goto-interface 'outline-path-completion)
(setq org-html-text-markup-alist
      '((bold . "<b>%s</b>")
        (code . "<code>%s</code>")
        (italic . "<i>%s</i>")
        (strike-through . "<strong style=\"color : red;\">%s</strong>")
        (underline . "<span class=\"underline\">%s</span>")
        (verbatim . "<code>%s</code>")))
(setq org-hugo-default-section-directory "post")
(setq org-image-actual-width '(400))
(setq org-latex-compiler "xelatex")
(setq org-latex-custom-lang-environments nil)
(setq org-latex-tables-booktabs t)
(setq org-latex-text-markup-alist
      '((bold . "\\textbf{%s}")
        (code . protectedtexttt)
        (italic . "\\emph{%s}")
        (strike-through . "\\emph{%s}")
        (underline . "\\uline{%s}")
        (verbatim . protectedtexttt)))
(setq org-mime-beautify-quoted-mail t)
(setq org-outline-path-complete-in-steps nil)
(setq org-preview-latex-default-process 'imagemagick)
(setq org-ref-bibliography-notes "/home/amos/Papers/notes.org")
(setq org-ref-default-bibliography '("/home/amos/git/serverconfig/amosbird.bib"))
(setq org-ref-open-pdf-function (lambda (fpath) (start-process "zathura" "*helm-bibtex-zathura*" "/usr/bin/zathura" fpath)))
(setq org-ref-pdf-directory "~/Papers/")
(setq org-src-block-faces '(("c++" default)))
(setq org-src-tab-acts-natively t)
(setq org-startup-folded nil)
(setq org-twbs-text-markup-alist
      '((bold . "<b>%s</b>")
        (code . "<code>%s</code>")
        (italic . "<i>%s</i>")
        (strike-through . "<strong style=\"color : red;\">%s</strong>")
        (underline . "<span class=\"underline\">%s</span>")
        (verbatim . "<code>%s</code>")))
(setq osc-http-addr (concat (string-trim-right (shell-command-to-string "ztaddr")) ":8866"))
(setq org-use-fast-todo-selection nil)
(setq package-check-signature nil)
(setq password-cache-expiry nil)
(setq process-environment initial-environment)
(setq projectile-find-dir-includes-top-level t)
(setq projectile-globally-ignored-directories '(".idea" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" "~/.emacs.d/.local/" ".sync" ".cquery_cached_index"))
(setq projectile-project-root-files-bottom-up '(".ignore" ".project" ".projectile" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs"))
(setq projectile-require-project-root t)
(setq projectile-sort-order 'recentf)
(setq query-replace-skip-read-only t)
(setq bibtex-completion-cite-prompt-for-optional-arguments nil)
(setq bibtex-completion-format-citation-functions
      '((org-mode      . +amos-bibtex-completion-format-citation-cite)
        (latex-mode    . +amos-bibtex-completion-format-citation-cite)
        (markdown-mode . bibtex-completion-format-citation-pandoc-citeproc)
        (default       . bibtex-completion-format-citation-default)))
(setq ivy-bibtex-default-action 'ivy-bibtex-insert-citation)

;; (setq redisplay-dont-pause t)
(setq require-final-newline t)
(setq save-interprogram-paste-before-kill nil)
(setq shell-file-name "/bin/bash")
(setq show-paren-priority -50)
(setq show-trailing-whitespace t)
(setq-default sp-autoskip-closing-pair t)
(setq sp-escape-quotes-after-insert nil)
(setq split-height-threshold nil)
(setq split-width-threshold 0)
(setq tab-always-indent t)
(setq truncate-partial-width-windows nil)
(setq user-full-name "Amos Bird")
(setq user-mail-address "amosbird@gmail.com")
(setq visible-cursor nil)
(setq visual-fill-column-center-text t)
(setq warning-suppress-types '((yasnippet backquote-change)))
(setq ws-butler-keep-whitespace-before-point nil)
(setq xref-after-jump-hook nil)
(setq yas-triggers-in-field nil)
(setq yasdcv-sdcv-command "sdcv --non-interactive --utf8-output --utf8-input \"%word\"")
(setq yasdcv-sdcv-dicts '(("jianminghy" "简明汉英词典" "powerword2007" t)))
(setq +latex-bibtex-file "~/git/serverconfig/amosbird.bib")
(setq-default line-spacing 0.1)
(setq reftex-label-ignored-macros-and-environments '("enumerate" "itemize"))
(setq reftex-insert-label-flags '("sft"))
(setq bibtex-completion-display-formats '((t . "${author:36} ${title:100} ${note:*}")))
(setq bibtex-completion-additional-search-fields '("note"))
(setq initial-buffer-choice (lambda () (get-buffer "*Messages*")))
(defun hide-mode-line-mode(&rest_)) ;; never hide modeline

(after! core-keybinds
  (add-to-list 'doom-evil-state-alist '(?s . sticky))
  (add-to-list 'doom-evil-state-alist '(?t . struct)))

(advice-add #'doom-init-clipboard-in-tty-emacs-h :override #'ignore)

(defun +amos*+evil-collection-init (orig-fun module &optional disabled-list)
  (unless (memq (or (car-safe module) module) '(edebug dired mu4e mu4e-conversation))
    (apply orig-fun (list module disabled-list))))
(advice-add #'+evil-collection-init :around #'+amos*+evil-collection-init)

(ignore-errors
  (define-category ?U "Uppercase")
  (define-category ?u "Lowercase")
  (modify-category-entry (cons ?A ?Z) ?U)
  (modify-category-entry (cons ?a ?z) ?u))

(require 'server)
(setq server-name (getenv "EMACS_SERVER_NAME"))
(if (not server-name) (setq server-name "server"))
(add-hook! 'emacs-startup-hook
  (defun +amos-emacs-lock-file-h()
    (with-temp-file (concat "/tmp/emacs-" server-name)
      (erase-buffer)
      (insert (number-to-string (emacs-pid))))))
(unless (server-running-p server-name)
  (server-start))
;; disable this fucking stupid feature by masking
(provide 'smartparens-lua)
