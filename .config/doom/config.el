;;; private/amos/config.el -*- lexical-binding: t; no-byte-compile: t; -*-

(add-to-list '+evil-collection-disabled-list 'mu4e 'mu4e-conversation)

(load! "+bindings")
(require 'dash)
(require 'evil-multiedit)
(require 'company)
(require 'company-lsp)
(require 'counsel)

(defun +amos/recenter (&rest _)
  (interactive)
  (recenter)
  (+nav-flash/blink-cursor))

(defvar +amos-dir (file-name-directory load-file-name))
(defvar +amos-snippets-dir (expand-file-name "snippets/" +amos-dir))
;; Don't use default snippets, use mine.
(after! yasnippet
  (add-hook! 'yas-minor-mode-hook (yas-activate-extra-mode 'fundamental-mode))
  (setq yas-snippet-dirs
        (append (list '+amos-snippets-dir '+file-templates-dir)
                (delq 'yas-installed-snippets-dir yas-snippet-dirs))))

(add-to-list 'auto-mode-alist '("/git/serverconfig/scripts/.+" . sh-mode))
(setq +file-templates-alist
      `(;; General
        ("/git/serverconfig/scripts/.+" :mode sh-mode)
        ("/git/serverconfig/.config/fish/functions/.+" :trigger "__func" :mode fish-mode)
        (gitignore-mode)
        (dockerfile-mode)
        ("/docker-compose\\.yml$" :mode yaml-mode)
        ("/Makefile$"             :mode makefile-gmake-mode)
        ;; elisp
        ("/.dir-locals.el$")
        ("/packages\\.el$" :when +file-templates-in-emacs-dirs-p
         :trigger "__doom-packages"
         :mode emacs-lisp-mode)
        ("/doctor\\.el$" :when +file-templates-in-emacs-dirs-p
         :trigger "__doom-doctor"
         :mode emacs-lisp-mode)
        ("/test/.+\\.el$" :when +file-templates-in-emacs-dirs-p
         :trigger "__doom-test"
         :mode emacs-lisp-mode)
        ("\\.el$" :when +file-templates-in-emacs-dirs-p
         :trigger "__doom-module"
         :mode emacs-lisp-mode)
        ("-test\\.el$" :mode emacs-ert-mode)
        (emacs-lisp-mode :trigger "__initfile")
        (snippet-mode)
        ;; C/C++
        ("/main\\.c\\(?:c\\|pp\\)$"   :trigger "__main.cpp"    :mode c++-mode)
        ("/win32_\\.c\\(?:c\\|pp\\)$" :trigger "__winmain.cpp" :mode c++-mode)
        ("\\.c\\(?:c\\|pp\\)$"        :trigger "__cpp" :mode c++-mode)
        ("\\.h\\(?:h\\|pp\\|xx\\)$"   :trigger "__hpp" :mode c++-mode)
        ("\\.h$" :trigger "__h" :mode c-mode)
        (c-mode  :trigger "__c")
        ;; go
        ("/main\\.go$" :trigger "__main.go" :mode go-mode :project t)
        (go-mode :trigger "__.go")
        ;; web-mode
        ("/normalize\\.scss$" :trigger "__normalize.scss" :mode scss-mode)
        ("/master\\.scss$" :trigger "__master.scss" :mode scss-mode)
        ("\\.html$" :trigger "__.html" :mode web-mode)
        (scss-mode)
        ;; java
        ("/main\\.java$" :trigger "__main" :mode java-mode)
        ("/build\\.gradle$" :trigger "__build.gradle" :mode android-mode)
        ("/src/.+\\.java$" :mode java-mode)
        ;; javascript
        ("/package\\.json$"        :trigger "__package.json" :mode json-mode)
        ("/bower\\.json$"          :trigger "__bower.json" :mode json-mode)
        ("/gulpfile\\.js$"         :trigger "__gulpfile.js" :mode js-mode)
        ("/webpack\\.config\\.js$" :trigger "__webpack.config.js" :mode js-mode)
        ("\\.js\\(?:on\\|hintrc\\)$" :mode json-mode)
        ;; Lua
        ("/main\\.lua$" :trigger "__main.lua" :mode love-mode)
        ("/conf\\.lua$" :trigger "__conf.lua" :mode love-mode)
        ;; Markdown
        (markdown-mode)
        ;; Org
        ("/README\\.org$"
         :when +file-templates-in-emacs-dirs-p
         :trigger "__doom-readme"
         :mode org-mode)
        ("\\.org$" :trigger "__" :mode org-mode)
        ;; PHP
        ("\\.class\\.php$" :trigger "__.class.php" :mode php-mode)
        (php-mode)
        ;; Python
        ;; TODO ("tests?/test_.+\\.py$" :trigger "__" :mode nose-mode)
        ;; TODO ("/setup\\.py$" :trigger "__setup.py" :mode python-mode)
        (python-mode)
        ;; Ruby
        ("/lib/.+\\.rb$"      :trigger "__module"   :mode ruby-mode :project t)
        ("/spec_helper\\.rb$" :trigger "__helper"   :mode rspec-mode :project t)
        ("_spec\\.rb$"                              :mode rspec-mode :project t)
        ("/\\.rspec$"         :trigger "__.rspec"   :mode rspec-mode :project t)
        ("\\.gemspec$"        :trigger "__.gemspec" :mode ruby-mode :project t)
        ("/Gemfile$"          :trigger "__Gemfile"  :mode ruby-mode :project t)
        ("/Rakefile$"         :trigger "__Rakefile" :mode ruby-mode :project t)
        (ruby-mode)
        ;; Rust
        ("/Cargo.toml$" :trigger "__Cargo.toml" :mode rust-mode)
        ("/main\\.rs$" :trigger "__main.rs" :mode rust-mode)
        ;; Slim
        ("/\\(?:index\\|main\\)\\.slim$" :mode slim-mode)
        ;; Shell scripts
        ("\\.zunit$" :trigger "__zunit" :mode sh-mode)
        (fish-mode)
        (sh-mode)
        ;; Solidity
        (solidity-mode :trigger "__sol")))

(setq epa-file-encrypt-to user-mail-address
      c-tab-always-indent t
      auth-sources (list (expand-file-name ".authinfo.gpg" +amos-dir)))

(defun +amos*no-authinfo-for-tramp (orig-fn &rest args)
  "Don't look into .authinfo for local sudo TRAMP buffers."
  (let ((auth-sources (if (equal tramp-current-method "sudo") nil auth-sources)))
    (apply orig-fn args)))
(advice-add #'tramp-read-passwd :around #'+amos*no-authinfo-for-tramp)

(defmacro +amos-make-special-indent-fn! (keyword)
  `(let* ((sname (symbol-name ,keyword))
          (s (intern (concat "special-indent-fn-" sname))))
     (fset s (lambda (pos state)
               (save-excursion
                 (search-backward sname)
                 (current-column))))
     (put ,keyword 'lisp-indent-function s)))
(setq +amos-list-starters
      '(:hint
        :textDocument
        ))
(--map (+amos-make-special-indent-fn! it) +amos-list-starters)
(put :color 'lisp-indent-function 'defun)
(put :pre 'lisp-indent-function 'defun)
(put :post 'lisp-indent-function 'defun)

(after! evil-snipe (evil-snipe-mode -1))

(after! evil-multiedit
  (setq evil-multiedit-follow-matches t))

(after! smartparens
  ;; Auto-close more conservatively
  (let ((unless-list '(sp-point-before-word-p
                       sp-point-after-word-p
                       sp-point-before-same-p)))
    (sp-pair "'"  nil :unless unless-list)
    (sp-pair "\"" nil :unless unless-list))
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p)))

(defun col-at-point (point)
  (save-excursion (goto-char point) (current-column)))

(defun evil--mc-make-cursor-at-col-append (_startcol endcol orig-line)
  (end-of-line)
  (when (> endcol (current-column))
    (insert-char ?\s (- endcol (current-column))))
  (move-to-column endcol)
  (unless (= (line-number-at-pos) orig-line)
    (evil-mc-make-cursor-here)))

(defun evil--mc-make-cursor-at-col-insert (startcol _endcol orig-line)
  (end-of-line)
  (move-to-column startcol)
  (unless (or (= (line-number-at-pos) orig-line) (> startcol (current-column)))
    (evil-mc-make-cursor-here)))

(defun evil--mc-make-vertical-cursors (beg end func)
  (evil-mc-pause-cursors)
  (apply-on-rectangle func
                      beg end (line-number-at-pos (point)))
  (evil-mc-resume-cursors)
  (evil-insert-state))

(defun evil-mc-insert-vertical-cursors (beg end)
  (interactive (list (region-beginning) (region-end)))
  (evil--mc-make-vertical-cursors beg end 'evil--mc-make-cursor-at-col-insert)
  (move-to-column (min (col-at-point beg) (col-at-point end))))

(defun evil-mc-append-vertical-cursors (beg end)
  (interactive (list (region-beginning) (region-end)))
  (when (and (evil-visual-state-p)
             (eq (evil-visual-type) 'line))
    (message "good")
    (let ((column (max (evil-column evil-visual-beginning)
                       (evil-column evil-visual-end))))
      (evil-visual-rotate 'upper-left)
      (move-to-column column t))
    )
  (evil--mc-make-vertical-cursors beg end 'evil--mc-make-cursor-at-col-append)
  (move-to-column (max (col-at-point beg) (col-at-point end))))

(after! evil-mc
  (nconc evil-mc-known-commands
         '((+amos/forward-delete-word . ((:default . evil-mc-execute-default-call)))
           (evil-repeat . ((:default . evil-mc-execute-default-call)))
           (+amos/smart-eol-insert . ((:default . evil-mc-execute-default-call)))
           (company-complete-common . ((:default . evil-mc-execute-default-complete)))
           (company-select-next . ((:default . evil-mc-execute-default-complete)))
           (company-select-previous . ((:default . evil-mc-execute-default-complete)))
           (+amos/forward-delete-word . ((:default . evil-mc-execute-default-call)))
           (+amos/backward-delete-word . ((:default . evil-mc-execute-default-call)))
           (+amos/forward-delete-subword . ((:default . evil-mc-execute-default-call)))
           (+amos/backward-delete-subword . ((:default . evil-mc-execute-default-call)))
           (+amos/delete-char . ((:default . evil-mc-execute-default-call)))
           (+amos/backward-delete-char . ((:default . evil-mc-execute-default-call)))
           (+amos/kill-line . ((:default . evil-mc-execute-default-call)))
           (+amos/backward-kill-to-bol-and-indent . ((:default . evil-mc-execute-default-call)))
           (+amos/replace-last-sexp . ((:default . evil-mc-execute-default-call)))
           (+amos/backward-word-insert . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
           (+amos/forward-word-insert . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
           (+amos/backward-subword-insert . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
           (+amos/forward-subword-insert . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
           (+amos/evil-backward-subword-begin . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
           (+amos/evil-forward-subword-begin . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))
           (+amos/evil-forward-subword-end . ((:default . evil-mc-execute-default-call-with-count) (visual . evil-mc-execute-visual-text-object)))))

  ;; if I'm in insert mode, chances are I want cursors to resume
  (add-hook! 'evil-mc-before-cursors-created
    (add-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors nil t))
  (add-hook! 'evil-mc-after-cursors-deleted
    (remove-hook 'evil-insert-state-entry-hook #'evil-mc-resume-cursors t)))

(after! cus-edit (evil-set-initial-state 'Custom-mode 'normal))
(after! ivy (evil-set-initial-state 'ivy-occur-grep-mode 'normal))
(after! compile (evil-set-initial-state 'compilation-mode 'normal))

(defun +amos|init-frame (&optional frame)
  (when (and frame (display-graphic-p frame))
    (with-selected-frame frame
      (dolist (charset '(kana han cjk-misc bopomofo))
        (set-fontset-font t charset
                          (font-spec :family "WenQuanYi Micro Hei" :size 14)))
      (remove-hook! 'after-make-frame-functions #'+amos|init-frame))))

(add-hook! 'after-init-hook
  (if initial-window-system
      (+amos|init-frame)
    (add-hook! 'after-make-frame-functions #'+amos|init-frame)))

(use-package osc
  :demand
  :init
  (defun +amos/other-window ()
    (interactive)
    (if (display-graphic-p)
        (i3-nav-right)
      (osc-nav-right)))
  (setq interprogram-cut-function 'osc-select-text
        browse-url-browser-function (lambda (url &optional _new-window)
                                      (if (display-graphic-p)
                                          (if _new-window
                                              (browse-url-chrome url)
                                            (browse-url-firefox url))
                                        (browse-url-osc url _new-window)))))

(def-package! realign-mode
  :commands realign-mode realign-windows
  :config
  ;; (add-hook! 'realign-hooks #'recenter)
  (defun amos-special-window-p (window)
    (let* ((fname (frame-parameter nil 'name))
           (buffer (window-buffer window))
           (buffname (string-trim (buffer-name buffer))))
      (or (equal buffname "*doom*")
          (equal buffname "*flycheck-posframe-buffer*")
          (equal fname "popup")
          (equal (with-current-buffer buffer major-mode) 'pdf-view-mode))))
  (push #'amos-special-window-p realign-ignore-window-predicates))

(setq recenter-redisplay nil)
(remove-hook! 'kill-emacs-query-functions #'doom-quit-p)
(remove-hook! 'doom-init-ui-hook #'blink-cursor-mode)
(remove-hook! 'doom-real-buffer-functions #'doom-dired-buffer-p)
;; (remove-hook! 'doom-init-ui-hook #'show-paren-mode)
(add-hook! 'doom-post-init-hook
  (realign-mode)
  (blink-cursor-mode -1)
  (setq-default truncate-lines nil))

(let ((evil-cursors '(("normal" "#b8860b" box)
                      ("insert" "#66cd00" bar)
                      ("emacs" "#7ec0ee" box)
                      ("replace" "#cd6600" hbar)
                      ("visual" "#808080" hbar)
                      ("motion" "#cd96cd" box)
                      ("lisp" "#ff6eb4" bar)
                      ("iedit" "#ff3030" box)
                      ("multiedit" "#ff3030" box)
                      ("multiedit-insert" "#ff3030" bar)
                      ("iedit-insert" "#ff3030" bar))))
  (cl-loop for (state color cursor) in evil-cursors
           do (set (intern (format "evil-%s-state-cursor" state)) (list color cursor))))

;; may delete the real hyphens
(defadvice fill-delete-newlines (before *amos+fill-delete-newlines activate)
  "Replace -\\n with an empty string when calling `fill-paragraph'."
  (when (eq this-command 'unfill-paragraph)
    (goto-char (ad-get-arg 0))
    (while (search-forward "-\n" (ad-get-arg 1) t)
      (replace-match "")
      (ad-set-arg 1 (- (ad-get-arg 1) 2)))))

(def-package! narrow-reindent
  :demand
  :config
  (defun narrow-reindent-mode-maybe ()
    (if (not (minibufferp))
        (narrow-reindent-mode +1)))
  (define-global-minor-mode global-narrow-reindent-mode
    narrow-reindent-mode narrow-reindent-mode-maybe
    :group 'narrow-reindent)
  (global-narrow-reindent-mode +1))

(def-package! git-gutter
  :config
  (defface +amos:modified
    '((t (:foreground "chocolate" :weight bold :inherit default)))
    "Face of modified")

  (defface +amos:added
    '((t (:foreground "ForestGreen" :weight bold :inherit default)))
    "Face of added")

  (defface +amos:deleted
    '((t (:foreground "DarkRed" :weight bold :inherit default)))
    "Face of deleted")

  (global-git-gutter-mode +1)
  (advice-add #'git-gutter:set-window-margin :override #'ignore)
  (defun +amos*git-gutter:before-string (sign)
    (let* ((gutter-sep (concat " " (make-string (- (car (window-margins)) 2) ? ) sign))
           (face (pcase sign
                   ("=" '+amos:modified)
                   ("+" '+amos:added)
                   ("-" '+amos:deleted)))
           (ovstring (propertize gutter-sep 'face face)))
      (propertize " " 'display `((margin left-margin) ,ovstring))))
  (advice-add #'git-gutter:put-signs :before (lambda (&rest _) (realign-windows)))
  (advice-add #'git-gutter:before-string :override #'+amos*git-gutter:before-string)
  (add-hook 'window-configuration-change-hook #'git-gutter:update-all-windows)
  )

(def-package! evil-textobj-line
  :after evil)

(unless window-system
  (require 'evil-terminal-cursor-changer)
  (xterm-mouse-mode +1)
  ;; enable terminal scroll
  (global-set-key (kbd "<mouse-6>")
                  (lambda ()
                    (interactive)
                    (evil-scroll-column-left 3)))
  (global-set-key (kbd "<mouse-7>")
                  (lambda ()
                    (interactive)
                    (evil-scroll-column-right 3)))
  (global-set-key (kbd "<mouse-4>")
                  (lambda ()
                    (interactive)
                    (evil-scroll-line-up 3)))
  (global-set-key (kbd "<mouse-5>")
                  (lambda ()
                    (interactive)
                    (evil-scroll-line-down 3)))
  (etcc-on))

(def-package! chinese-yasdcv
  :commands yasdcv-translate-at-point)

(def-package! counsel-dash
  :commands counsel-dash
  :init
  (setq
   counsel-dash-docsets-path "~/.docsets"
   counsel-dash-docsets-url "https://raw.github.com/Kapeli/feeds/master"
   counsel-dash-min-length 2
   counsel-dash-candidate-format "%d %n (%t)"
   counsel-dash-enable-debugging t
   counsel-dash-browser-func (lambda (url) (interactive) (browse-url url t))
   counsel-dash-ignored-docsets nil)
  (defun counsel-dash-at-point ()
    (interactive)
    (counsel-dash (thing-at-point 'symbol)))
  (add-hook! fish-mode (setq-local helm-dash-docsets '("fish" "Linux_Man_Pages")))
  (add-hook! sh-mode (setq-local helm-dash-docsets '("Bash" "Linux_Man_Pages")))
  (add-hook! go-mode (setq-local helm-dash-docsets '("Go")))
  (add-hook! cmake-mode (setq-local helm-dash-docsets '("CMake")))
  (add-hook! java-mode (setq-local helm-dash-docsets '("Java")))
  (add-hook! rust-mode (setq-local helm-dash-docsets '("Rust")))
  (add-hook! lua-mode (setq-local helm-dash-docsets '("Lua_5.1")))
  (add-hook! c-mode (setq-local helm-dash-docsets '("C" "Linux_Man_Pages")))
  (add-hook! c++-mode (setq-local helm-dash-docsets '("C" "C++" "Linux_Man_Pages" "Boost")))
  (add-hook! python-mode (setq-local helm-dash-docsets '("Python_3" "Python_2")))
  (add-hook! emacs-lisp-mode (setq-local helm-dash-docsets '("Emacs_Lisp"))))

(setq-hook! 'lua-mode-hook flycheck-highlighting-mode 'lines)

(defun advice-browse-url (ofun &rest candidate)
  (if (boundp 'amos-browse)
      (apply 'browse-url-firefox candidate)
    (apply ofun candidate)))
(advice-add 'browse-url :around #'advice-browse-url)

(defadvice run-skewer (around +amos*run-skewer activate)
  (setq-local amos-browse t)
  ad-do-it)

(after! ediff
  (add-hook! 'ediff-keymap-setup-hook
    (define-key ediff-mode-map "k" 'ediff-previous-difference)
    (define-key ediff-mode-map "j" 'ediff-next-difference)))

(def-package! easy-hugo
  :commands easy-hugo
  :config
  (evil-set-initial-state 'easy-hugo-mode 'emacs)
  (add-hook! 'easy-hugo-mode-hook (setq-local amos-browse t))
  (setq
   easy-hugo-basedir "~/sites/blog"
   easy-hugo-url "https://wentropy.com"
   easy-hugo-sshdomain "blog"
   easy-hugo-root "/var/www/blog/"
   easy-hugo-previewtime "300"
   easy-hugo-default-ext ".org"))

(def-package! lispyville
  :commands lispyville-mode)

(def-package! move-text
  :commands move-text-up move-text-down)

(def-package! ws-butler
  :demand
  :config
  (ws-butler-global-mode))

(defun my-create-newline-and-enter-sexp (&rest _ignored)
  "Open a new brace or bracket expression, with relevant newlines and indent."
  (newline)
  (indent-according-to-mode)
  (forward-line -1)
  (indent-according-to-mode))

(with-eval-after-load 'smartparens
  (sp-local-pair 'c-mode "{" nil :post-handlers
                 '((my-create-newline-and-enter-sexp "RET")))
  (sp-local-pair 'java-mode "{" nil :post-handlers
                 '((my-create-newline-and-enter-sexp "RET"))))

;; stole from https://emacs.stackexchange.com/a/16495/16662
(defmacro doom-with-advice (args &rest body)
  (declare (indent 3))
  (let ((fun-name (car args))
        (advice   (cadr args))
        (orig-sym (make-symbol "orig")))
    `(cl-letf* ((,orig-sym  (symbol-function ',fun-name))
                ((symbol-function ',fun-name)
                 (lambda (&rest args)
                   (apply ,advice ,orig-sym args))))
       ,@body)))

(defadvice edebug-pop-to-buffer (around +amos*edebug-pop-to-buffer activate)
  (doom-with-advice (split-window (lambda (orig-fun window) (funcall orig-fun window nil 'right)))
      ad-do-it))

(defadvice hl-line-mode (after +amos*hl-line-mode activate)
  (set-face-background hl-line-face "Gray13"))

(defun +amos/evil-undefine ()
  (interactive)
  (let (evil-mode-map-alist)
    (call-interactively (key-binding (this-command-keys)))))

(defun +amos*switch-buffer-matcher (regexp candidates)
  "Return REGEXP matching CANDIDATES.
Skip buffers that match `ivy-ignore-buffers'."
  (let ((res (ivy--re-filter regexp candidates)))
    (if (or (null ivy-use-ignore)
            (null ivy-ignore-buffers)
            (string-match "\\`\\." ivy-text))
        res
      (or (cl-remove-if
           (lambda (buf)
             (cl-find-if
              (lambda (f-or-r)
                (if (functionp f-or-r)
                    (funcall f-or-r buf)
                  (string-match-p f-or-r buf)))
              ivy-ignore-buffers))
           res)
          (and (eq ivy-use-ignore t)
               res)))))
(advice-add #'ivy--switch-buffer-matcher :override #'+amos*switch-buffer-matcher)

(def-package! hl-line+
  :demand
  :config
  (global-hl-line-mode +1))

(after! evil
  (defadvice evil-ret-gen (around amos*evil-ret-gen activate)
    (let ((url (thing-at-point 'url)))
      (if url (goto-address-at-point)
        ad-do-it))))

(def-package! unfill
  :commands (unfill-region unfill-paragraph unfill-toggle)
  :init
  (global-set-key [remap fill-paragraph] #'unfill-toggle))

;; recenter buffer when switching windows
(defun +amos|update-window-buffer-list ()
  (walk-window-tree
   (lambda (window)
     (let ((old-buffer (window-parameter window 'my-last-buffer))
           (new-buffer (window-buffer window)))
       (unless (eq old-buffer new-buffer)
         ;; The buffer of a previously existing window has changed or
         ;; a new window has been added to this frame.
         ;; (+amos/recenter)
         (setf (window-parameter window 'my-last-buffer) new-buffer))))))
(add-hook! 'window-configuration-change-hook #'+amos|update-window-buffer-list)

(defun +amos/counsel-rg-projectile ()
  (interactive)
  (unless (doom-project-p)
    (user-error "You’re not in a project"))
  (counsel-rg nil (doom-project-root)))

(defun +amos/counsel-rg-cur-dir ()
  (interactive)
  (counsel-rg nil default-directory))

(def-package! yapfify
  :after python)

;; from spacemacs
(defun +amos/rename-current-buffer-file (&optional arg)
  "Rename the current buffer and the file it is visiting.
If the buffer isn't visiting a file, ask if it should
be saved to a file, or just renamed.

If called without a prefix argument, the prompt is
initialized with the current filename."
  (interactive "P")
  (let* ((name (buffer-name))
         (filename (buffer-file-name)))
    (if (and filename (file-exists-p filename))
        ;; the buffer is visiting a file
        (let* ((dir (file-name-directory filename))
               (new-name (read-file-name "New name: " (if arg dir filename))))
          (cond ((get-buffer new-name)
                 (error "A buffer named '%s' already exists!" new-name))
                (t
                 (let ((dir (file-name-directory new-name)))
                   (when (and (not (file-exists-p dir))
                              (yes-or-no-p
                               (format "Create directory '%s'?" dir)))
                     (make-directory dir t)))
                 (rename-file filename new-name 1)
                 (rename-buffer new-name)
                 (set-visited-file-name new-name)
                 (set-buffer-modified-p nil)
                 (when (fboundp 'recentf-add-file)
                   (recentf-add-file new-name)
                   (recentf-remove-if-non-kept filename))
                 (when (and (featurep 'projectile)
                            (projectile-project-p))
                   (call-interactively #'projectile-invalidate-cache))
                 (message "File '%s' successfully renamed to '%s'"
                          name (file-name-nondirectory new-name)))))
      ;; the buffer is not visiting a file
      (let ((key))
        (while (not (memq key '(?s ?r)))
          (setq key (read-key (propertize
                               (format
                                (concat "Buffer '%s' is not visiting a file: "
                                        "[s]ave to file or [r]ename buffer?")
                                name) 'face 'minibuffer-prompt)))
          (cond ((eq key ?s)            ; save to file
                 ;; this allows for saving a new empty (unmodified) buffer
                 (unless (buffer-modified-p) (set-buffer-modified-p t))
                 (save-buffer))
                ((eq key ?r)            ; rename buffer
                 (let ((new-name (read-string "New buffer name: ")))
                   (while (get-buffer new-name)
                     ;; ask to rename again, if the new buffer name exists
                     (if (yes-or-no-p
                          (format (concat "A buffer named '%s' already exists: "
                                          "Rename again?") new-name))
                         (setq new-name (read-string "New buffer name: "))
                       (keyboard-quit)))
                   (rename-buffer new-name)
                   (message "Buffer '%s' successfully renamed to '%s'"
                            name new-name)))
                ;; ?\a = C-g, ?\e = Esc and C-[
                ((memq key '(?\a ?\e)) (keyboard-quit))))))))

;; BEGIN align functions
(defun +amos/align-repeat-left (start end regexp)
  (interactive "r\nsAlign regexp: ")
  (+amos/align-repeat start end regexp))
(defun +amos/align-repeat-right (start end regexp)
  (interactive "r\nsAlign regexp: ")
  (+amos/align-repeat start end regexp t t))
;; modified function from http://emacswiki.org/emacs/AlignCommands
(defun +amos/align-repeat (start end regexp &optional justify-right after)
  "Repeat alignment with respect to the given regular expression.
If JUSTIFY-RIGHT is non nil justify to the right instead of the
left. If AFTER is non-nil, add whitespace to the left instead of
the right."
  (let* ((ws-regexp (if (string-empty-p regexp)
                        "\\(\\s-+\\)"
                      "\\(\\s-*\\)"))
         (complete-regexp (if after
                              (concat regexp ws-regexp)
                            (concat ws-regexp regexp)))
         (group (if justify-right -1 1)))
    (unless (use-region-p)
      (save-excursion
        (while (and
                (string-match-p complete-regexp (thing-at-point 'line))
                (= 0 (forward-line -1)))
          (setq start (point-at-bol))))
      (save-excursion
        (while (and
                (string-match-p complete-regexp (thing-at-point 'line))
                (= 0 (forward-line 1)))
          (setq end (point-at-eol)))))
    (align-regexp start end complete-regexp group 1 t)))

(defun +amos/dos2unix ()
  "Converts the current buffer to UNIX file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-unix nil))

(defun +amos/unix2dos ()
  "Converts the current buffer to DOS file format."
  (interactive)
  (set-buffer-file-coding-system 'undecided-dos nil))

;; from https://www.emacswiki.org/emacs/CopyingWholeLines
(defun +amos/duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ; Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ; Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ; Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ; Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ; Save column
        (if (> 0 n)                             ; Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(defun +amos/uniquify-lines ()
  "Remove duplicate adjacent lines in a region or the current buffer"
  (interactive)
  (save-excursion
    (save-restriction
      (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
             (beg (if region-active (region-beginning) (point-min)))
             (end (if region-active (region-end) (point-max))))
        (goto-char beg)
        (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
          (replace-match "\\1"))))))

(defun +amos/sort-lines (&optional reverse)
  "Sort lines in a region or the current buffer.
A non-nil argument sorts in reverse order."
  (interactive "P")
  (let* ((region-active (or (region-active-p) (evil-visual-state-p)))
         (beg (if region-active (region-beginning) (point-min)))
         (end (if region-active (region-end) (point-max))))
    (sort-lines reverse beg end)))

(defun +amos/sort-lines-reverse ()
  "Sort lines in reverse order, in a region or the current buffer."
  (interactive)
  (+amos/sort-lines -1))

(defun +amos/sort-lines-by-column (&optional reverse)
  "Sort lines by the selected column,
using a visual block/rectangle selection.
A non-nil argument sorts in REVERSE order."
  (interactive "P")
  (if (and
       ;; is there an active selection
       (or (region-active-p) (evil-visual-state-p))
       ;; is it a block or rectangle selection
       (or (eq evil-visual-selection 'block) (eq rectangle-mark-mode t))
       ;; is the selection height 2 or more lines
       (>= (1+ (- (line-number-at-pos (region-end))
                  (line-number-at-pos (region-beginning)))) 2))
      (sort-columns reverse (region-beginning) (region-end))
    (error "Sorting by column requires a block/rect selection on 2 or more lines.")))

(defun +amos/sort-lines-by-column-reverse ()
  "Sort lines by the selected column in reverse order,
using a visual block/rectangle selection."
  (interactive)
  (+amos/sort-lines-by-column -1))

(defun swap-args (fun)
  (if (not (equal (interactive-form fun)
                  '(interactive "P")))
      (error "Unexpected")
    (advice-add
     fun
     :around
     (lambda (x &rest args)
       "Swap the meaning the universal prefix argument"
       (if (called-interactively-p 'any)
           (apply x (cons (not (car args)) (cdr args)))
         (apply x args))))))

(after! evil-surround
  (setq-default evil-surround-pairs-alist (append '((?` . ("`" . "`")) (?~ . ("~" . "~"))) evil-surround-pairs-alist)))

(def-package! gitattributes-mode
  :defer t)

(def-package! gitconfig-mode
  :defer t)

(def-package! gitignore-mode
  :defer t)

;; way slower
;; (def-package! magit-svn
;;   :commands turn-on-magit-svn
;;   :init (add-hook 'magit-mode-hook 'turn-on-magit-svn))

(def-package! page-break-lines
  :commands global-page-break-lines-mode
  :init
  (global-page-break-lines-mode +1))

(defun amos*page-break-lines--update-display-tables  (&optional _)
  "Function called for updating display table in windows of current selected frame."
  (unless (minibufferp)
    (mapc 'page-break-lines--update-display-table (window-list nil 'no-minibuffer))))
(advice-add 'page-break-lines--update-display-tables :override #'amos*page-break-lines--update-display-tables)

(def-package! adoc-mode
  :mode "\\.adoc$")

(defun +amos*ivy-rich-switch-buffer-pad (str len &optional left)
  "Improved version of `ivy-rich-switch-buffer-pad' that truncates long inputs."
  (let ((real-len (length str)))
    (cond
     ((< real-len len) (if left
                           (concat (make-string (- len real-len) ? ) str)
                         (concat str (make-string (- len real-len) ? ))))
     ((= len real-len) str)
     ((< len 1) str)
     (t (concat (substring str 0 (- len 1)) "…")))))

;; Override the original function using advice
(advice-add 'ivy-rich-switch-buffer-pad :override #'+amos*ivy-rich-switch-buffer-pad)

(evil-define-motion +amos*evil-beginning-of-line ()
  "Move the cursor to the beginning of the current line."
  :type exclusive
  (doom/backward-to-bol-or-indent))
(advice-add 'evil-beginning-of-line :override #'+amos*evil-beginning-of-line)

(defun +amos/save-buffer-without-dtw ()
  (interactive)
  (let ((b (current-buffer)))   ; memorize the buffer
    (with-temp-buffer ; new temp buffer to bind the global value of before-save-hook
      (let ((before-save-hook (remove 'delete-trailing-whitespace before-save-hook)))
        (with-current-buffer b  ; go back to the current buffer, before-save-hook is now buffer-local
          (let ((before-save-hook (remove 'delete-trailing-whitespace before-save-hook)))
            (save-buffer)))))))

(defun +amos/counsel-projectile-switch-project ()
  (interactive)
  (require 'counsel-projectile)
  (ivy-read (projectile-prepend-project-name "Switch to project: ")
            projectile-known-projects
            :preselect (and (projectile-project-p)
                            (abbreviate-file-name (projectile-project-root)))
            :action #'+amos/find-file
            :require-match t
            :caller #'+amos/counsel-projectile-switch-project))

(defun +amos/projectile-current-project-files ()
  "Return a list of files for the current project."
  (let* ((directory (projectile-project-root))
         (files (and projectile-enable-caching
                     (gethash directory projectile-projects-cache))))
    ;; nothing is cached
    (unless files
      (when projectile-enable-caching
        (message "Empty cache. Projectile is initializing cache..."))
      (setq files
            (projectile-adjust-files
             directory
             (projectile-project-vcs directory)
             (split-string
              (shell-command-to-string
               (concat "cd " (projectile-project-root) "; fd --hidden -E '.git'")) "\n")))
      ;; cache the resulting list of files
      (when projectile-enable-caching
        (projectile-cache-project (projectile-project-root) files)))
    (projectile-sort-files files)))

(defun +amos/projectile-find-file (&optional arg)
  "Jump to a file in the current project.

With a prefix ARG, invalidate the cache first."
  (interactive "P")
  (require 'counsel-projectile)
  (projectile-maybe-invalidate-cache arg)
  (ivy-read (projectile-prepend-project-name "Find file: ")
            (+amos/projectile-current-project-files)
            ;; (projectile-current-project-files)
            :matcher counsel-projectile-find-file-matcher
            :require-match t
            :sort t
            :action counsel-projectile-find-file-action
            :caller #'+amos/projectile-find-file))

(advice-add #'projectile-cache-files-find-file-hook :override #'ignore)
;; TODO prevent projectile clear caches when removing files, following doesn't work anymore
;; (add-hook! 'projectile-mode-hook (ad-deactivate 'delete-file))
(after! projectile
  (advice-remove 'delete-file #'delete-file-projectile-remove-from-cache))

(defvar switch-buffer-functions
  nil
  "A list of functions to be called when the current buffer has been changed.
Each is passed two arguments, the previous buffer and the current buffer.")

(defvar switch-buffer-functions--last-buffer
  nil
  "The last current buffer.")

(defvar switch-buffer-functions--running-p
  nil
  "Non-nil if currently inside of run `switch-buffer-functions-run'.")

(defun switch-buffer-functions-run ()
  "Run `switch-buffer-functions' if needed.
This function checks the result of `current-buffer', and run
`switch-buffer-functions' when it has been changed from
the last buffer.
This function should be hooked to `buffer-list-update-hook'."
  (unless switch-buffer-functions--running-p
    (let ((switch-buffer-functions--running-p t)
          (current (current-buffer))
          (previous switch-buffer-functions--last-buffer))
      (unless (eq previous
                  current)
        (run-hook-with-args 'switch-buffer-functions
                            previous
                            current)
        (setq switch-buffer-functions--last-buffer
              (current-buffer))))))

(add-hook! 'buffer-list-update-hook #'switch-buffer-functions-run)

(defun endless/sharp ()
  "Insert #' unless in a string or comment."
  (interactive)
  (call-interactively #'self-insert-command)
  (let ((ppss (syntax-ppss)))
    (unless (or (elt ppss 3)
                (elt ppss 4)
                (eq (char-after) ?'))
      (insert "'"))))

(add-hook! 'after-save-hook #'executable-make-buffer-file-executable-if-script-p)

(def-package! go-playground
  :commands (go-playground go-playground-mode)
  :bind (:map go-playground-mode-map
          ("<f8>" . go-playground-rm)))

(def-package! rust-playground
  :commands (rust-playground rust-playground-mode)
  :bind (:map rust-playground-mode-map
          ("<f8>" . rust-playground-rm)))

(def-package! cc-playground
  :commands cc-playground cc-playground-mode cc-playground-find-snippet cc-playground-leetcode
  :load-path (lambda () (interactive) (if (string= (system-name) "t450s") "~/git/cc-playground"))
  :init
  (put 'cc-exec 'safe-local-variable #'stringp)
  (put 'cc-flags 'safe-local-variable #'stringp)
  (put 'cc-links 'safe-local-variable #'stringp)
  (dolist (x '(cc-playground-exec cc-playground-debug cc-playground-exec-test cc-playground-bench))
    (advice-add x :before #'evil-normal-state))
  :bind (:map cc-playground-mode-map
          ("<f8>" . cc-playground-rm) ; terminal
          ("S-RET" . cc-playground-rm) ; gui
          ("C-c r" . cc-playground-add-or-modify-tag)
          ("C-c b" . cc-playground-bench)
          ("C-c d" . cc-playground-debug)
          ("C-c t" . cc-playground-debug-test)
          ("C-c l" . cc-playground-ivy-add-library-link)
          ("C-c c" . cc-playground-change-compiler)
          ("C-c o" . cc-playground-switch-optimization-flag)
          ("C-c f" . cc-playground-add-compilation-flags)))

(def-package! py-playground
  :commands py-playground py-playground-mode py-playground-find-snippet
  :load-path (lambda () (interactive) (if (string= (system-name) "t450s") "~/git/py-playground"))
  :init
  (dolist (x '(py-playground-exec py-playground-debug))
    (advice-add x :before #'evil-normal-state))
  :bind (:map py-playground-mode-map
          ("<f8>" . py-playground-rm) ; terminal
          ("S-RET" . py-playground-rm) ; gui
          ("C-c r" . py-playground-add-or-modify-tag)
          ("C-c d" . py-playground-debug)))

(defvar +amos--ivy-regex-hash
  (make-hash-table :test #'equal)
  "Store pre-computed regex.")

(defun +amos*ivy-regex-half-quote (str &optional greedy)
  "Re-build regex pattern from STR in case it has a space.
When GREEDY is non-nil, join words in a greedy way."
  (let ((hashed (unless greedy
                  (gethash str +amos--ivy-regex-hash))))
    (if hashed
        (prog1 (cdr hashed)
          (setq ivy--subexps (car hashed)))
      (when (string-match "\\([^\\]\\|^\\)\\\\$" str)
        (setq str (substring str 0 -1)))
      (cdr (puthash str
                    (let ((subs (ivy--split str)))
                      (if (= (length subs) 1)
                          (cons (setq ivy--subexps 0) (regexp-quote (car subs)))
                        (cons (setq ivy--subexps (length subs))
                              (mapconcat (lambda (s) (format "\\(%s\\)" (regexp-quote s))) subs (if greedy ".*" ".*?")))))
                    +amos--ivy-regex-hash)))))

(defun +amos--old-ivy-regex (str &optional greedy)
  "Re-build regex pattern from STR in case it has a space.
When GREEDY is non-nil, join words in a greedy way."
  (let ((hashed (unless greedy
                  (gethash str ivy--regex-hash))))
    (if hashed
        (prog1 (cdr hashed)
          (setq ivy--subexps (car hashed)))
      (when (string-match "\\([^\\]\\|^\\)\\\\$" str)
        (setq str (substring str 0 -1)))
      (cdr (puthash str
                    (let ((subs (ivy--split str)))
                      (if (= (length subs) 1)
                          (cons
                           (setq ivy--subexps 0)
                           (car subs))
                        (cons
                         (setq ivy--subexps (length subs))
                         (mapconcat
                          (lambda (x)
                            (if (string-match "\\`\\\\([^?].*\\\\)\\'" x)
                                x
                              (format "\\(%s\\)" x)))
                          subs
                          (if greedy
                              ".*"
                            ".*?")))))
                    ivy--regex-hash)))))

(defvar +amos--old-ivy-regex-function '+amos--old-ivy-regex)
(defun +amos*ivy-toggle-regexp-quote ()
  "Toggle the regexp quoting."
  (interactive)
  (setq ivy--old-re nil)
  (cl-rotatef ivy--regex-function +amos--old-ivy-regex-function ivy--regexp-quote))

(advice-add #'ivy-toggle-regexp-quote :override #'+amos*ivy-toggle-regexp-quote)
(advice-add #'ivy--regex :override #'+amos*ivy-regex-half-quote)

(def-package! rainbow-mode)

(def-package! google-translate
  :commands google-translate-at-point google-translate-query-translate)

(def-package! kurecolor
  :after rainbow-mode
  :config
  ;; | color    | toggle                     | meaning      |
  ;; |----------+----------------------------+--------------|
  ;; | red      |                            | persist      |
  ;; | blue     | :exit t                    | transient    |
  ;; | amaranth | :foreign-keys warn         | persist w    |
  ;; | teal     | :foreign-keys warn :exit t | transient w  |
  ;; | pink     | :foreign-keys run          | nested       |
  (defhydra +rgb@kurecolor (:color red :hint nil)
    "
Inc/Dec      _w_/_W_ brightness      _d_/_D_ saturation      _e_/_E_ hue    "
    ("w" kurecolor-decrease-brightness-by-step)
    ("W" kurecolor-increase-brightness-by-step)
    ("d" kurecolor-decrease-saturation-by-step)
    ("D" kurecolor-increase-saturation-by-step)
    ("e" kurecolor-decrease-hue-by-step)
    ("E" kurecolor-increase-hue-by-step)
    ("q" nil "cancel" :color blue)))

(evil-define-command ab-char-inc ()
  (save-excursion
    (let ((chr  (1+ (char-after))))
      (unless (characterp chr) (error "Cannot increment char by one"))
      (delete-char 1)
      (insert chr))))

(evil-define-command +amos/replace-last-sexp ()
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%S" value))))

(evil-define-command +amos/replace-defun ()
  (narrow-to-defun)
  (goto-char (point-max))
  (let ((value (eval (preceding-sexp))))
    (kill-sexp -1)
    (insert (format "%s" value)))
  (widen)
  (backward-char))

(evil-define-command +amos/new-empty-elisp-buffer ()
  (let ((buf (generate-new-buffer "*new*")))
    (+amos/workspace-new)
    (switch-to-buffer buf)
    (emacs-lisp-mode)
    (evil-insert-state)
    (setq buffer-offer-save nil)
    buf))

(evil-define-command +amos/ivy-complete-dir ()
  (let ((enable-recursive-minibuffers t)
        (history (+amos--get-all-jump-dirs))
        (old-last ivy-last)
        (ivy-recursive-restore nil))
    (ivy-read "Choose-directory: "
              history
              :action (lambda (x)
                        (setq x (concat x "/"))
                        (ivy--reset-state
                         (setq ivy-last old-last))
                        (delete-minibuffer-contents)
                        (insert (substring-no-properties x))
                        (ivy--cd-maybe)))))

(defun +amos/smart-jumper (&optional f)
  (unless (or (eq last-command '+amos/smart-jumper-backward)
              (eq last-command '+amos/smart-jumper-forward))
    (evil-set-jump))
  (let* ((dir (if f -1 (forward-char) 1))
         (c (point))
         quote
         delim)
    (setq quote (if (nth 3 (syntax-ppss))
                    (let ((b (save-excursion (progn (up-list -1 t t) (point))))
                          (e (save-excursion (progn (up-list 1 t t) (point)))))
                      (if (and (< b c) (< c e))
                          (if (< 0 dir) e b)
                        (user-error "syntax-ppss says point is in quotes but we cannot locate the boundary.")))
                  -1))
    (if (< 0 quote)
        (goto-char quote)
      (let* ((paren (save-excursion (if (= 0 (evil-up-paren ?\( ?\) dir)) (point) nil)))
             (bracket (save-excursion (if (= 0 (evil-up-paren ?\[ ?\] dir)) (point) nil)))
             (brace (save-excursion (if (= 0 (evil-up-paren ?{ ?} dir)) (point) nil))))
        (setq delim (condition-case nil
                        (if (< dir 0)
                            (-max (--filter it (list paren bracket brace)))
                          (-min (--filter it (list paren bracket brace))))
                      (error nil))))
      (if delim (goto-char delim)))
    (if (< 0 dir) (backward-char))))

(evil-define-command +amos/smart-jumper-backward ()
  (+amos/smart-jumper t))

(evil-define-command +amos/smart-jumper-forward ()
  (+amos/smart-jumper))

(evil-define-command +amos/complete ()
  (require 'thingatpt)
  (require 'company)
  (if (/= (point)
          (save-excursion
            (if (bounds-of-thing-at-point 'symbol)
                (end-of-thing 'symbol))
            (point)))
      (save-excursion (insert " ")))
  (company-manual-begin))

(evil-define-command +amos/complete-filter ()
  (+amos/complete)
  (company-filter-candidates))

(defvar my-kill-ring nil)
(defmacro mkr! (&rest body)
  `(let (interprogram-cut-function
         (kill-ring my-kill-ring))
     ,@body
     (setq my-kill-ring kill-ring)))

(evil-define-command +amos/delete-char()
  (mkr! (delete-char 1 1)))

(evil-define-command +amos/backward-delete-char()
  (mkr! (backward-delete-char-untabify 1 1)))

(evil-define-command +amos/kill-line ()
  (mkr! (kill-region (point)
                     (let ((overlay (iedit-find-overlay-at-point (point) 'iedit-occurrence-overlay-name)))
                       (if overlay
                           (overlay-end overlay)
                         (point-at-eol))))))

(evil-define-command +amos/backward-kill-to-bol-and-indent ()
  (if (bolp) (+amos/backward-delete-char)
    (let* ((overlay (iedit-find-overlay-at-point (1- (point)) 'iedit-occurrence-overlay-name))
           (x (if overlay (overlay-start overlay) (save-excursion (doom/backward-to-bol-or-indent))))
           (y (point)))
      (mkr! (kill-region x y)))))

(defun +amos-insert-state-p ()
  (or (evil-insert-state-p) (evil-multiedit-insert-state-p) (active-minibuffer-window)))

(defun +amos-insert-state ()
  (if (evil-multiedit-state-p)
      (evil-multiedit-insert-state)
    (evil-insert-state)))

(defmacro +amos-subword-move! (type command)
  `(evil-define-motion ,(intern (concat "+amos/" (s-replace "word" "subword" (symbol-name command)))) (count)
     :type ,type
     (let ((find-word-boundary-function-table +amos-subword-find-word-boundary-function-table))
       (,command count))))

(+amos-subword-move! inclusive evil-forward-word-end)
(+amos-subword-move! exclusive evil-forward-word-begin)
(+amos-subword-move! exclusive evil-backward-word-begin)

(evil-define-command +amos/forward-delete-word (&optional subword)
  (evil-signal-at-bob-or-eob 1)
  (when (or (not (or (evil-multiedit-state-p) (evil-multiedit-insert-state-p)))
            (iedit-find-overlay-at-point (point) 'iedit-occurrence-overlay-name))
    (unless (+amos-insert-state-p)
      (+amos-insert-state))
    (mkr! (kill-region (point)
                       (max
                        (save-excursion
                          (if (looking-at "[ \t\r\n\v\f]")
                              (progn
                                (re-search-forward "[^ \t\r\n\v\f]")
                                (backward-char))
                            (+amos-word-movement-internal subword 1))
                          (point))
                        (line-beginning-position))))))
(evil-define-command +amos/forward-delete-subword ()
  (+amos/forward-delete-word t))

(evil-define-command +amos/backward-delete-word (&optional subword)
  (evil-signal-at-bob-or-eob -1)
  (when (or (not (or (evil-multiedit-state-p) (evil-multiedit-insert-state-p)))
            (iedit-find-overlay-at-point (1- (point)) 'iedit-occurrence-overlay-name))
    (unless (or (eolp) (+amos-insert-state-p))
      (+amos-insert-state)
      (forward-char))
    (mkr! (kill-region (point)
                       (min
                        (save-excursion
                          (if (looking-back "[ \t\r\n\v\f]")
                              (progn
                                (re-search-backward "[^ \t\r\n\v\f]")
                                (forward-char))
                            (+amos-word-movement-internal subword -1))
                          (point))
                        (line-end-position))))))
(evil-define-command +amos/backward-delete-subword ()
  (+amos/backward-delete-word t))

(evil-define-command +amos/backward-word-insert (&optional subword)
  (evil-signal-at-bob-or-eob -1)
  (unless (or (eolp) (+amos-insert-state-p))
    (+amos-insert-state)
    (forward-char))
  (if (looking-back "[ \t\r\n\v\f]")
      (progn
        (re-search-backward "[^ \t\r\n\v\f]")
        (forward-char))
    (+amos-word-movement-internal subword -1)))
(evil-define-command +amos/backward-subword-insert ()
  (+amos/backward-word-insert t))

(evil-define-command +amos/forward-word-insert (&optional subword)
  (evil-signal-at-bob-or-eob 1)
  (unless (+amos-insert-state-p)
    (+amos-insert-state))
  (if (looking-at "[ \t\r\n\v\f]")
      (progn
        (re-search-forward "[^ \t\r\n\v\f]")
        (backward-char))
    (+amos-word-movement-internal subword 1)))
(evil-define-command +amos/forward-subword-insert ()
  (+amos/forward-word-insert t))

(defvar +amos-subword-forward-regexp
  "\\W*\\(\\([[:upper:]]*\\(\\W\\)?\\)[[:lower:][:digit:]]*\\)"
  "Regexp used by `subword-forward-internal'.")

(defvar +amos-subword-backward-regexp
  "\\(\\(\\W\\|[[:lower:][:digit:]]\\)\\([[:upper:]]+\\W*\\)\\|\\W\\w+\\)"
  "Regexp used by `subword-backward-internal'.")

(defun +amos-subword-forward-internal ()
  (if (and
       (save-excursion
         (let ((case-fold-search nil))
           (modify-syntax-entry ?_ "_")
           (re-search-forward +amos-subword-forward-regexp nil t)))
       (> (match-end 0) (point)))
      (goto-char
       (cond
        ((and (< 1 (- (match-end 2) (match-beginning 2)))
              (not (and (null (match-beginning 3))
                        (eq (match-end 2) (match-end 1)))))
         (1- (match-end 2)))
        (t
         (match-end 0))))
    (forward-word 1)))

(defun +amos-subword-backward-internal ()
  (if (save-excursion
        (let ((case-fold-search nil))
          (modify-syntax-entry ?_ "_")
          (re-search-backward +amos-subword-backward-regexp nil t)))
      (goto-char
       (cond
        ((and (match-end 3)
              (< 1 (- (match-end 3) (match-beginning 3)))
              (not (eq (point) (match-end 3))))
         (1- (match-end 3)))
        (t
         (1+ (match-beginning 0)))))
    (backward-word 1)))

(defconst +amos-subword-find-word-boundary-function-table
  (let ((tab (make-char-table nil)))
    (set-char-table-range tab t #'+amos-subword-find-word-boundary)
    tab))

(defconst +amos-subword-empty-char-table
  (make-char-table nil))

(defun +amos-subword-find-word-boundary (pos limit)
  (let ((find-word-boundary-function-table +amos-subword-empty-char-table))
    (save-match-data
      (save-excursion
        (save-restriction
          (if (< pos limit)
              (progn
                (goto-char pos)
                (narrow-to-region (point-min) limit)
                (+amos-subword-forward-internal))
            (goto-char (1+ pos))
            (narrow-to-region limit (point-max))
            (+amos-subword-backward-internal))
          (point))))))

(defun +amos-word-movement-internal (subword dir)
  (let ((find-word-boundary-function-table
         (if subword
             +amos-subword-find-word-boundary-function-table
           +amos-subword-empty-char-table)))
    (goto-char
     (funcall (if (< 0 dir) #'min #'max)
              (save-excursion
                (let ((word-separating-categories evil-cjk-word-separating-categories)
                      (word-combining-categories evil-cjk-word-combining-categories))
                  (if subword (push '(?u . ?U) word-separating-categories)))
                (forward-word dir)
                (point))
              (save-excursion
                (if (< 0 dir)
                    (if-let ((overlay (iedit-find-overlay-at-point (point) 'iedit-occurrence-overlay-name)))
                        (goto-char (overlay-end overlay))
                      (goto-char (line-end-position)))
                  (if-let ((overlay (iedit-find-overlay-at-point (1- (point)) 'iedit-occurrence-overlay-name)))
                      (goto-char (overlay-start overlay))
                    (goto-char (line-beginning-position))))
                (point))
              (save-excursion
                (forward-thing 'evil-word dir)
                (point))))))

(defun +amos-word-movement-internal (subword dir)
  (let ((find-word-boundary-function-table
         (if subword
             +amos-subword-find-word-boundary-function-table
           +amos-subword-empty-char-table)))
    (evil-forward-nearest
     dir
     (lambda (&optional cnt)
       (let ((word-separating-categories evil-cjk-word-separating-categories)
             (word-combining-categories evil-cjk-word-combining-categories))
         (if subword (push '(?u . ?U) word-separating-categories))
         (pnt (point)))
       (forward-word cnt)
       (if (= pnt (point)) cnt 0))
     (lambda (&optional cnt)
       (if (< 0 cnt)
           (if-let ((overlay (iedit-find-overlay-at-point (point) 'iedit-occurrence-overlay-name)))
               (goto-char (overlay-end overlay))
             (goto-char (line-end-position)))
         (if-let ((overlay (iedit-find-overlay-at-point (1- (point)) 'iedit-occurrence-overlay-name)))
             (goto-char (overlay-start overlay))
           (goto-char (line-beginning-position))))
       0)
     (lambda (&optional cnt) (forward-thing 'evil-word cnt) 0))))

(evil-define-text-object +amos/any-object-inner (count &optional beg end type)
  (save-excursion
    (if (looking-at "['\"]")
        (if (nth 3 (syntax-ppss))
            (backward-char)
          (forward-char))
      (if (looking-at "[[({]")
          (forward-char)))
    (+amos/smart-jumper-backward)
    (forward-char)
    (let ((s (point))
          (e (progn
               (backward-char)
               (+amos/smart-jumper-forward)
               (point))))
      (evil-range s e type :expanded t))))

(evil-define-text-object +amos/any-object-outer (count &optional beg end type)
  (save-excursion
    (if (looking-at "['\"]")
        (if (nth 3 (syntax-ppss))
            (backward-char)
          (forward-char))
      (if (looking-at "[[({]")
          (forward-char)))
    (+amos/smart-jumper-backward)
    (push-mark)
    (let ((s (point))
          (e (progn
               (+amos/smart-jumper-forward)
               (forward-char)
               (point))))
      (evil-range s e type :expanded t))))

(def-package! subword
  :commands subword-forward subword-backward)

(after! xref
  (add-to-list 'xref-prompt-for-identifier '+lookup/definition :append)
  (add-to-list 'xref-prompt-for-identifier '+lookup/references :append)
  (add-to-list 'xref-prompt-for-identifier 'xref-find-references :append))

(def-package! lsp-mode
  :init
  (setq lsp-eldoc-hook nil
        lsp-prefer-flymake nil
        lsp-enable-indentation nil
        lsp-auto-guess-root t)
  :config
  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu))

(cl-defun +amos-lsp-find-custom (kind method &optional extra &key display-action)
  "Send request named METHOD and get cross references of the symbol under point.
EXTRA is a plist of extra parameters."
  (let ((loc (lsp-request method
                          (append (lsp--text-document-position-params) extra))))
    (if loc
        (+amos-ivy-xref (lsp--locations-to-xref-items (if (sequencep loc) loc (list loc))) (symbol-name kind))
      (message "Not found for: %s" (thing-at-point 'symbol t)))))

(defun +amos/definitions ()
  (interactive)
  (+amos-lsp-find-custom
   'definitions "textDocument/definition"))

(defun +amos/references ()
  (interactive)
  (+amos-lsp-find-custom
   'references "textDocument/references"))

(def-package! lsp-ui
  :config
  (setq lsp-ui-sideline-enable nil)
  )

(defun +amos/create-fish-function (name)
  (interactive "sNew function's name: ")
  (let ((full-name (expand-file-name (concat name ".fish") "/home/amos/.config/fish/functions/")))
    (if (file-exists-p full-name)
        (user-error "Function with the same name already exists!"))
    (find-file full-name)
    (evil-initialize-state 'insert)))


(defvar zygospore-spore-formation-register-name
  "zygospore-windows-time-machine"
  "Name of the register that zygospore uses to reverse `zygospore-delete-other-windows'.")

(defvar zygospore-last-full-frame-window
  nil
  "Last window that was full-frame'd.")

(defvar zygospore-last-full-frame-buffer
  nil
  "Last buffer that was full-frame'd.")

(defun zygospore-delete-other-window ()
  "Save current window-buffer configuration and full-frame the current buffer."
  (setq zygospore-last-full-frame-window (selected-window))
  (setq zygospore-last-full-frame-buffer (current-buffer))
  (window-configuration-to-register zygospore-spore-formation-register-name)
  (delete-other-windows))

(defun zygospore-restore-other-windows ()
  "Restore the window configuration to prior to full-framing."
  (jump-to-register zygospore-spore-formation-register-name))

(defun zygospore-toggle-delete-other-windows ()
  "Main zygospore func.
If the current frame has several windows, it will act as `delete-other-windows'.
If the current frame has one window,
  and it is the one that was last full-frame'd,
  and the buffer remained the same,
it will restore the window configuration to prior to full-framing."
  (interactive)
  (if (and (equal (selected-window) (next-window))
           (equal (selected-window) zygospore-last-full-frame-window)
           (equal (current-buffer) zygospore-last-full-frame-buffer))
      (zygospore-restore-other-windows)
    (zygospore-delete-other-window)))

(defun save-buffer-maybe ()
  (interactive)
  (when (and (buffer-file-name)
             (not defining-kbd-macro)
             (buffer-modified-p))
    (save-buffer))
  nil)

(setq ccls-enabled nil)
(defun diagnostic-maybe ()
  (interactive)
  (when ccls-enabled
    (ccls/diagnostic))
  nil)

(defun git-gutter-maybe ()
  (interactive)
  (when git-gutter-mode (ignore (git-gutter)))
  nil)

(add-hook 'doom-escape-hook #'save-buffer-maybe)
(add-hook 'doom-escape-hook #'diagnostic-maybe)
(add-hook 'doom-escape-hook #'git-gutter-maybe)

(defun my-reload-dir-locals-for-all-buffer-in-this-directory ()
  "For every buffer with the same `default-directory` as the
current buffer's, reload dir-locals."
  (interactive)
  (let ((dir default-directory))
    (dolist (buffer (buffer-list))
      (with-current-buffer buffer
        (when (equal default-directory dir))
        (my-reload-dir-locals-for-current-buffer)))))

(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(add-hook 'emacs-lisp-mode-hook
          (defun enable-autoreload-for-dir-locals ()
            (when (and (buffer-file-name)
                       (equal dir-locals-file
                              (file-name-nondirectory (buffer-file-name))))
              (add-hook (make-variable-buffer-local 'after-save-hook)
                        'my-reload-dir-locals-for-all-buffer-in-this-directory))))

(advice-remove #'counsel-ag-function #'+ivy*counsel-ag-function)

(defun +amos*helm-dash-result-url (docset-name filename &optional anchor)
  "Return the full, absolute URL to documentation.
Either a file:/// URL joining DOCSET-NAME, FILENAME & ANCHOR with sanitization
 of spaces or a http(s):// URL formed as-is if FILENAME is a full HTTP(S) URL."
  (let* ((clean-filename (replace-regexp-in-string "<dash_entry_.*>" "" filename))
         (path (format "%s%s" clean-filename (if anchor (replace-regexp-in-string "%2E" "%252E" (format "#%s" anchor)) ""))))
    (if (string-match-p "^https?://" path)
        path
      (replace-regexp-in-string
       " "
       "%20"
       (concat
        "file:///"
        (expand-file-name "Contents/Resources/Documents/" (helm-dash-docset-path docset-name))
        path)))))
(advice-add #'helm-dash-result-url :override #'+amos*helm-dash-result-url)

(setq tmux-p (getenv "TMUX"))
(setq gui-p (getenv "GUI"))

(if tmux-p
    (advice-add #'switch-to-buffer-other-frame :override #'+amos/switch-to-buffer-other-frame))

(defvar +amos-last-xref-list nil)
(defun +amos/ivy-xref-make-collection (xrefs)
  "Transform XREFS into a collection for display via `ivy-read'."
  (let (collection last-xref-list)
    (dolist (xref xrefs)
      (with-slots (summary location) xref
        (let ((line (xref-location-line location))
              (file (xref-location-group location))
              (candidate nil))
          (setq candidate (concat
                           ;; use file name only
                           (car (reverse (split-string file "\\/")))
                           (when (string= "integer" (type-of line))
                             (concat ":" (int-to-string line) ": "))
                           summary))
          (push `(,candidate . ,location) collection)
          (push (format "%s:%d:%s" (replace-regexp-in-string (concat "^" default-directory) "./" file) line summary) last-xref-list))))
    (setq +amos-last-xref-list (nreverse last-xref-list))
    (nreverse collection)))

(defun +amos-ivy-xref (xrefs kind)
  (if (= 1 (length xrefs))
      (dolist (xref xrefs)
        (with-slots (summary location) xref
          (let* ((marker (xref-location-marker location))
                 (buf (marker-buffer marker)))
            (evil-set-jump)
            (switch-to-buffer buf)
            (goto-char marker)
            (evil-set-jump)
            (+amos/recenter))))
    (let ((xref-pos (point))
          (xref-buffer (current-buffer))
          (default-directory (doom-project-root))
          (success nil))
      (ivy-read (concat "Find " kind ": ") (+amos/ivy-xref-make-collection xrefs)
                :unwind (lambda ()
                          (unless success
                            (switch-to-buffer xref-buffer)
                            (goto-char xref-pos)
                            (+amos/recenter)))
                :action (lambda (x)
                          (let ((location (cdr x)))
                            (let* ((marker (xref-location-marker location))
                                   (buf (marker-buffer marker)))
                              (evil-set-jump)
                              (switch-to-buffer buf)
                              (with-ivy-window
                                (goto-char marker)
                                (+amos/recenter)
                                (evil-set-jump))
                              (unless (eq 'ivy-call this-command)
                                (setq success t)))))))))

(defun +amos*xref--find-xrefs (input kind arg display-action)
  (let ((xrefs (funcall (intern (format "xref-backend-%s" kind))
                        (xref-find-backend)
                        arg)))
    (unless xrefs
      (user-error "No %s found for: %s" (symbol-name kind) input))
    (+amos-ivy-xref xrefs kind)))

(advice-add #'xref--find-xrefs :override #'+amos*xref--find-xrefs)

(after! recentf
  (setq recentf-exclude '("/tmp/" "/ssh:" "\\.?ido\\.last$" "\\.revive$" "\\.git" "/TAGS$" "/var" "/usr" "~/cc/" "~/Mail/" "~/\\.emacs\\.d/.local")))

(after! ivy
  (setf (alist-get t ivy-re-builders-alist) 'ivy--regex-plus)
  (dolist (command '(ccls/includes))
    (setf (alist-get command ivy-re-builders-alist) 'ivy--regex-fuzzy))
  (defun amos-recentf ()
    (interactive)
    (require 'recentf)
    (recentf-mode)
    (ivy-read "Recentf: " (mapcar #'substring-no-properties recentf-list)
              :action (lambda (f)
                        (with-ivy-window
                          (find-file f)))
              :sort t
              :caller 'counsel-recentf))
  (ivy-set-actions
   'amos-recentf
   '(("j" find-file-other-window "other window")
     ("f" find-file-other-frame "other frame")
     ("x" counsel-find-file-extern "open externally")))

  ;; SLOWWWWW
  ;; (defun amos-recentf-sort-function (a b)
  ;;   (let ((project-root (doom-project-root)))
  ;;     (or (file-in-directory-p a project-root) (not (file-in-directory-p b project-root)))))
  ;; (add-to-list 'ivy-sort-functions-alist '(counsel-recentf . amos-recentf-sort-function))

  (dolist (cmd '(counsel-find-file +amos/counsel-projectile-switch-project))
    (ivy-add-actions
     cmd
     '(("f" find-file-other-frame "other frame"))))
  (dolist (cmd '(ivy-switch-buffer))
    (ivy-add-actions
     cmd
     '(("f" switch-to-buffer-other-frame "other frame")))))

(defun +amos/redisplay-and-recenter ()
  (interactive)
  (redraw-display)
  (+amos/recenter))

(defun evil-numbers/inc-at-pt (amount)
  "Increment the number at point or after point before `end-of-line' by AMOUNT."
  (interactive "p*")
  (save-match-data
    (when (evil-numbers/search-number))
    (or
     ;; find binary literals
     (evil-numbers/search-and-replace "0[bB][01]+" "01" "\\([01]+\\)" amount 2)
     ;; find octal literals
     (evil-numbers/search-and-replace "0[oO][0-7]+" "01234567" "\\([0-7]+\\)" amount 8)
     ;; find hex literals
     (evil-numbers/search-and-replace "0[xX][0-9a-fA-F]*"
                                      "0123456789abcdefABCDEF"
                                      "\\([0-9a-fA-F]+\\)" amount 16)
     ;; find decimal literals
     (progn
       (skip-chars-backward "0123456789")
       (skip-chars-backward "-")
       (when (looking-at "-?\\([0-9]+\\)")
         (replace-match
          (format (format "%%0%dd" (- (match-end 1) (match-beginning 1)))
                  (+ amount (string-to-number (match-string 0) 10))))
         ;; Moves point one position back to conform with Vim
         (forward-char -1)
         t)))))

(defun evil-numbers/dec-at-pt (amount)
  "Decrement the number at point or after point before `end-of-line' by AMOUNT."
  (interactive "p*")
  (evil-numbers/inc-at-pt (- amount)))

;;; utils

(defun evil-numbers/search-number ()
  "Return non-nil if a binary, oct, hex or decimal literal at or after point.
If point is already within or after a literal it stays.

The literals have to be in the following forms:
binary: 0[bB][01]+, e.g. 0b101 or 0B0
octal: 0[oO][0-7]+, e.g. 0o42 or 0O5
hexadecimal 0[xX][0-9a-fA-F]+, e.g. 0xBEEF or 0Xcafe
decimal: [0-9]+, e.g. 42 or 23"
  (or
   ;; numbers or format specifier in front
   (looking-back (rx (or (+? digit)
                         (and "0" (or (and (in "bB") (*? (in "01")))
                                      (and (in "oO") (*? (in "0-7")))
                                      (and (in "xX") (*? (in digit "A-Fa-f"))))))))
   ;; search for number in rest of line
   ;; match 0 of specifier or digit, being in a literal and after specifier is
   ;; handled above
   (and
    (re-search-forward "[[:digit:]]" (point-at-eol) t)
    (or
     (not (memq (char-after) '(?b ?B ?o ?O ?x ?X)))
     (/= (char-before) ?0)
     (and (> (point) 2)				; Should also take bofp into consideration
          (not (looking-back "\\W0" 2)))
     ;; skip format specifiers and interpret as bool
     (<= 0 (skip-chars-forward "bBoOxX"))))))

(defun evil-numbers/search-and-replace (look-back skip-back search-forward inc base)
  "When looking back at LOOK-BACK skip chars SKIP-BACK backwards and
replace number incremented by INC in BASE and return non-nil."
  (when (looking-back look-back)
    (skip-chars-backward skip-back)
    (search-forward-regexp search-forward)
    (replace-match (evil-numbers/format (+ inc (string-to-number (match-string 1) base))
                                        (length (match-string 1))
                                        base))
    ;; Moves point one position back to conform with Vim
    (forward-char -1)
    t))

(defun evil-numbers/format (num width base)
  "Format NUM with at least WIDTH space in BASE."
  (cond
   ((= base 2) (evil-numbers/format-binary num width))
   ((= base 8) (format (format "%%0%do" width) num))
   ((= base 16) (format (format "%%0%dX" width) num))
   (t "")))

(defun evil-numbers/format-binary (number &optional width fillchar)
  "Format NUMBER as binary.
Fill up to WIDTH with FILLCHAR (defaults to ?0) if binary
representation of `NUMBER' is smaller."
  (let (nums
        (fillchar (or fillchar ?0)))
    (while (> number 0)
      (push (number-to-string (% number 2)) nums)
      (setq number (truncate number 2)))
    (let ((len (length nums)))
      (apply #'concat
             (if (and width (< len width))
                 (make-string (- width len) fillchar)
               "")
             nums))))

(defun +amos/inc (s e &optional inc)
  (save-restriction
    (narrow-to-region s e)
    (goto-char (point-min))
    (if (and (evil-numbers/inc-at-pt +amos--gca-count) inc)
        (setq +amos--gca-count (+ 1 +amos--gca-count)))))

(defvar +amos--gca-count nil)
(defun +amos/gca (count start end)
  (interactive "*p\nr")
  (setq +amos--gca-count count)
  (evil-apply-on-block #'+amos/inc start end nil t))

(defun +amos/ca (count start end)
  (interactive "*p\nr")
  (setq +amos--gca-count count)
  (evil-apply-on-block #'+amos/inc start end nil))

(defun +amos/dec (s e &optional inc)
  (save-restriction
    (narrow-to-region s e)
    (goto-char (point-min))
    (if (and (evil-numbers/dec-at-pt +amos--gcd-count) inc)
        (setq +amos--gcd-count (+ 1 +amos--gcd-count)))))

(defvar +amos--gcd-count nil)
(defun +amos/gcd (count start end)
  (interactive "*p\nr")
  (setq +amos--gcd-count count)
  (evil-apply-on-block #'+amos/dec start end nil t))

(defun +amos/cd (count start end)
  (interactive "*p\nr")
  (setq +amos--gcd-count count)
  (evil-apply-on-block #'+amos/dec start end nil))

(def-package! direnv
  :config
  (setq direnv-show-paths-in-summary nil)
  (direnv-mode))
(add-hook! 'after-save-hook (if (string= (file-name-nondirectory buffer-file-name) ".envrc") (direnv-update-environment)))

(defun +amos/direnv-reload ()
  (interactive)
  (shell-command! "direnv allow")
  (direnv-update-environment)
  (kill-buffer " *direnv*")
  (direnv-mode +1))

(setq +amos-end-of-statement-regex nil)
(defun set-eos! (modes &rest plist)
  (dolist (mode (doom-enlist modes))
    (push (cons mode plist) +amos-end-of-statement-regex)))
(set-eos! '(c-mode c++-mode) :regex-char '("[ \t\r\n\v\f]" "[[{(;]" ?\;))
(set-eos! '(sql-mode) :regex-char '("[ \t\r\n\v\f]" ";" ?\;))
(set-eos! '(emacs-lisp-mode) :regex-char "[ \t\r\n\v\f]")

(defun +amos/maybe-add-end-of-statement (&optional move)
  (interactive)
  (if (and move (not (eolp))) (end-of-line)
    (let ((p (save-excursion
               (let (s e)
                 (back-to-indentation)
                 (setq s (point))
                 (end-of-line)
                 (setq e (point))
                 (when-let* ((plist (cdr (assq major-mode +amos-end-of-statement-regex))))
                   (when-let* ((regex-char (doom-enlist (plist-get plist :regex-char))))
                     (if (looking-back (car regex-char))
                         (delete-trailing-whitespace s e)
                       (when-let* ((chars (nth 1 regex-char))
                                   (char (nth 2 regex-char)))
                         (if (looking-back chars 1)
                             (if move (funcall-interactively (key-binding (kbd "RET"))))
                           (insert char))))))
                 (point)))))
      (if move (goto-char p)))))

(defun +amos/insert-eol-and-return (&optional move)
  (interactive)
  (let (s e)
    (end-of-line)
    (back-to-indentation)
    (setq s (point))
    (end-of-line)
    (setq e (point))
    (when-let* ((plist (cdr (assq major-mode +amos-end-of-statement-regex))))
      (when-let* ((regex-char (doom-enlist (plist-get plist :regex-char))))
        (if (looking-back (car regex-char))
            (delete-trailing-whitespace s e)
          (when-let* ((chars (nth 1 regex-char))
                      (char (nth 2 regex-char)))
            (unless (looking-back chars 1)
              (insert char))
            (funcall-interactively (key-binding (kbd "RET")))))))))

(defun +amos/smart-eol-insert ()
  (interactive)
  (+amos/maybe-add-end-of-statement t))

(defun +amos/mark-whole-buffer ()
  (interactive)
  (evil-visual-line (point-min) (point-max)))

(defun +amos/projectile-find-other-file ()
  (interactive)
  (if (and (boundp 'cc-playground-mode) cc-playground-mode)
      (cc-switch-between-src-and-test)
    (require 'projectile)
    (projectile-find-other-file)))

(setq interprogram-paste-function #'x-get-selection)
(advice-add #'hide-mode-line-mode :override #'ignore)

;; (defun +amos/fcitx--activate-proc ()
;;   (osc-fcitx-activate))

;; (defun +amos/fcitx--deactivate-proc ()
;;   (osc-fcitx-deactivate))

;; there is no easy way to query local info from remote via termio
;; (when tmux-p
;;   (advice-add #'fcitx-check-status :override (lambda () t))
;;   (advice-add #'fcitx--active-p :override #'ignore)
;;   (advice-add #'fcitx--activate-proc :override #'+amos/fcitx--activate-proc)
;;   (advice-add #'fcitx--deactivate-proc :override #'+amos/fcitx--deactivate-proc))

(require 'fcitx)
(fcitx-aggressive-setup)

(unless gui-p
  (defun +amos*fcitx--activate-proc ()
    (shell-command! "fcitxenable"))

  (defun +amos*fcitx--deactivate-proc ()
    (shell-command! "fcitxdisable"))

  (defun +amos*fcitx--active-p-proc ()
    (string= "2\n" (shell-command-to-string "fcitxstatus")))

  (advice-add #'fcitx--activate-proc :override #'+amos*fcitx--activate-proc)
  (advice-add #'fcitx--deactivate-proc :override #'+amos*fcitx--deactivate-proc)
  (advice-add #'fcitx--active-p-proc :override #'+amos*fcitx--active-p-proc))

(defun first-non-dired-buffer ()
  (--first (not (with-current-buffer it (derived-mode-p 'dired-mode))) (buffer-list)))

(setq +popup-default-alist
      '((slot . 1)
        (vslot . -1)
        (side . right)
        (size . 0.5)
        (reusable-frames . nil)))
(map-put +popup-default-parameters 'modeline t)

(set-popup-rules!
  '(("^\\*"  :slot 1 :vslot -1 :select t)
    ("^ \\*" :slot 1 :vslot -1 :size +popup-shrink-to-fit))
  '(("^\\*Completions"
     :slot -1 :vslot -2 :ttl 0)
    ("^\\*Warning*"
     :actions (+popup-display-buffer-stacked-side-window))
    ("^\\*git-gutter*"
     :side right :size 0.5)
    ("^\\*Flycheck"
     :side bottom :size 0.5 :select t :ttl 0 :quit t)
    ("^\\*Compil\\(?:ation\\|e-Log\\)"
     :side right :size 0.5 :select t :ttl 0 :quit t)
    ("^\\*\\(?:scratch\\|Messages\\)"
     :autosave t :ttl nil)
    ("^\\*Man "
     :size 0.45 :vslot -6 :ttl 0 :quit t :select t)
    ("^\\*doom \\(?:term\\|eshell\\)"
     :size 0.25 :vslot -10 :select t :quit nil :ttl 0)
    ("^\\*doom:"
     :vslot -20 :size 0.35 :autosave t :select t :modeline t :quit nil)
    ("^\\*\\(?:\\(?:Pp E\\|doom e\\)val\\)"
     :size +popup-shrink-to-fit :side right :ttl 0 :select ignore)
    ("^\\*Customize" :ignore t)
    ("^ \\*undo-tree\\*"
     :slot 2 :side left :size 20 :select t :quit t)
    ;; `help-mode', `helpful-mode'
    ("^\\*[Hh]elp"
     :side right :size 0.5 :select t)
    ;; `Info-mode'
    ("^\\*info\\*$"
     :slot 2 :vslot 2 :size 0.45 :select t)
    ("\\*TeX" :side right :size 0.4)
    ("^\\(?:\\*magit\\|magit:\\)" :ignore t)
    ("^\\*ivy-occur"
     :side right :size 0.9 :select t :ttl 0)
    ("\\[ Table \\]\\*"
     :side right :size 0.9 :select t :quit nil))
  '(("^\\*Backtrace" :side right :size 0.5 :quit nil)))

(evil-define-command +amos*evil-visual-paste (count &optional register)
  "Paste over Visual selection."
  :suppress-operator t
  (interactive "P<x>")
  ;; evil-visual-paste is typically called from evil-paste-before or
  ;; evil-paste-after, but we have to mark that the paste was from
  ;; visual state
  (setq this-command 'evil-visual-paste)
  (let* ((text (if register
                   (evil-get-register register)
                 (current-kill 0)))
         (yank-handler (car-safe (get-text-property
                                  0 'yank-handler text)))
         new-kill
         paste-eob)
    (evil-with-undo
      (let* ((kill-ring (list (current-kill 0)))
             (kill-ring-yank-pointer kill-ring))
        (when (evil-visual-state-p)
          (evil-visual-rotate 'upper-left)
          ;; if we replace the last buffer line that does not end in a
          ;; newline, we use `evil-paste-after' because `evil-delete'
          ;; will move point to the line above
          (when (and (= evil-visual-end (point-max))
                     (/= (char-before (point-max)) ?\n))
            (setq paste-eob t))
          (evil-delete evil-visual-beginning evil-visual-end
                       (evil-visual-type) ?_)
          (when (and (eq yank-handler #'evil-yank-line-handler)
                     (not (eq (evil-visual-type) 'line))
                     (not (= evil-visual-end (point-max))))
            (insert "\n"))
          (evil-normal-state)
          (setq new-kill (current-kill 0))
          (current-kill 1))
        (if paste-eob
            (evil-paste-after count register)
          (evil-paste-before count register)))
      (when evil-kill-on-visual-paste
        (kill-new new-kill))
      ;; mark the last paste as visual-paste
      (setq evil-last-paste
            (list (nth 0 evil-last-paste)
                  (nth 1 evil-last-paste)
                  (nth 2 evil-last-paste)
                  (nth 3 evil-last-paste)
                  (nth 4 evil-last-paste)
                  t)))))
(advice-add #'evil-visual-paste :override #'+amos*evil-visual-paste)

(defun +amos*+lookup-set-jump (orig-fun &rest args)
  (evil-set-jump)
  (condition-case nil
      (apply orig-fun args)
    (error nil))
  (condition-case nil
      (+amos/recenter)
    (error nil)))
(advice-add #'+lookup/definition :around #'+amos*+lookup-set-jump)
(advice-add #'+lookup/references :around #'+amos*+lookup-set-jump)

(defun +amos*evil--jumps-push ()
  (+amos-clean-evil-jump-list)
  (unless (eq this-command 'ivy-call)
    (let ((target-list (evil--jumps-get-window-jump-list)))
      (let ((file-name (buffer-file-name))
            (buffer-name (buffer-name))
            (current-pos (point-marker))
            (first-pos nil)
            (first-file-name nil)
            (excluded nil))
        (when (and (not file-name)
                   (string-match-p evil--jumps-buffer-targets buffer-name))
          (setq file-name buffer-name))
        (when file-name
          (dolist (pattern evil-jumps-ignored-file-patterns)
            (when (string-match-p pattern file-name)
              (setq excluded t)))
          (unless excluded
            (unless (ring-empty-p target-list)
              (setq first-pos (car (ring-ref target-list 0)))
              (setq first-file-name (car (cdr (ring-ref target-list 0)))))
            (unless (and (equal first-file-name file-name)
                         (save-excursion
                           (let ((a (progn (goto-char first-pos) (end-of-line) (point)))
                                 (b (progn (goto-char current-pos) (end-of-line) (point))))
                             (= a b))))
              (ring-insert target-list `(,current-pos ,file-name)))))))))
(advice-add #'evil--jumps-push :override #'+amos*evil--jumps-push)

(defun +amos*evil--jumps-jump (idx shift)
  (+amos-clean-evil-jump-list)
  (let ((target-list (evil--jumps-get-window-jump-list)))
    (let* ((current-file-name (or (buffer-file-name) (buffer-name)))
           (size (ring-length target-list)))
      ;; jump back to the idx line first, if already on the same line, shift
      (let* ((place (ring-ref target-list idx))
             (pos (car place))
             (target-file-name (cadr place)))
        (if (and (equal target-file-name current-file-name)
                 (save-excursion
                   (let* ((a (progn (end-of-line) (point)))
                          (b (progn (goto-char pos) (end-of-line) (point))))
                     (= a b))))
            (setq idx (+ idx shift))))
      (when (and (< idx size) (>= idx 0))
        ;; actual jump
        (run-hooks 'evil-jumps-pre-jump-hook)
        (let* ((place (ring-ref target-list idx))
               (pos (car place))
               (file-name (cadr place)))
          (setq evil--jumps-jumping t)
          (if (string-match-p evil--jumps-buffer-targets file-name)
              (switch-to-buffer file-name)
            (find-file file-name))
          (setq evil--jumps-jumping nil)
          (goto-char pos)
          (setf (evil-jumps-struct-idx (evil--jumps-get-current)) idx)
          (run-hooks 'evil-jumps-post-jump-hook))))))
(advice-add #'evil--jumps-jump :override #'+amos*evil--jumps-jump)

(defun +amos/yank-buffer-filename ()
  "Copy the current buffer's path to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (abbreviate-file-name filename)))
    (error "Couldn't find filename in current buffer")))

(defun +amos/yank-buffer-filename-nondir ()
  "Copy the current buffer's filename to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (file-name-nondirectory filename)))
    (error "Couldn't find filename in current buffer")))

(defun +amos/yank-buffer-filename-with-line-position ()
  "Copy the current buffer's filename with line number to the kill ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name (bound-and-true-p list-buffers-directory))))
      (message (kill-new (concat "b" filename ":" (number-to-string (line-number-at-pos)) "\n")))
    (error "Couldn't find filename in current buffer")))

(defun +amos/evil-insert-line-above (count)
  "Insert one or several lines above the current point's line without changing
the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-above))))

(defun +amos/evil-insert-line-below (count)
  "Insert one or several lines below the current point's line without changing
the current state and point position."
  (interactive "p")
  (dotimes (_ count) (save-excursion (evil-insert-newline-below))))

(defun +amos/copy-without-useless-indent-and-newline ()
  (interactive)
  (let ((inhibit-message t))
    (when (evil-visual-state-p)
      (call-interactively #'narrow-reindent-to-region)
      (set-mark (point-min))
      (goto-char (point-max))
      (backward-char)
      (end-of-line)
      (call-interactively #'copy-region-as-kill)
      (narrow-reindent-widen)
      (+amos/recenter))))

(defun +amos/evil-visual-insert-snippet ()
  (interactive)
  (let ((start (region-beginning))
        (end (region-end))
        (register 121))
    (setq yas--condition-cache-timestamp (current-time))
    (let* ((yas-wrap-around-region register)
           (templates (yas--all-templates (yas--get-snippet-tables)))
           (yas--current-template (and templates
                                       (or (and (cl-rest templates) ;; more than one template for same key
                                                (yas--prompt-for-template templates))
                                           (car templates))))
           (_ (evil-substitute start end 'line register))
           (where (cons (point) (point))))
      (with-temp-buffer
        (evil-paste-from-register register)
        (indent-region (point-min) (point-max))
        (goto-char (point-max))
        (backward-char)
        (end-of-line)
        (let ((text (filter-buffer-substring (point-min) (point))))
          (evil-set-register ?y text)))
      (if yas--current-template
          (progn
            (yas-expand-snippet (yas--template-content yas--current-template)
                                (car where)
                                (cdr where)
                                (yas--template-expand-env yas--current-template)))
        (yas--message 1 "No snippets can be inserted here!")))))

(defun async-shell-command! (command)
  (let ((inhibit-message t))
    (call-process-shell-command command nil 0)))

(defun shell-command! (command)
  (let ((inhibit-message t))
    (shell-command command)))

(defun +amos/tmux-detach ()
  "Detach if inside tmux."
  (interactive)
  (shell-command! "tmux detach-client"))

(defun +amos/find-file-other-frame (filename &optional wildcards)
  "Open file if inside tmux."
  (interactive
   (find-file-read-args "Find file in other frame: "
                        (confirm-nonexistent-file-or-buffer)))
  (+amos/workspace-new)
  (find-file filename wildcards))

(defun +amos/switch-to-buffer-other-frame (buffer-or-name &optional norecord)
  (interactive
   (list (read-buffer-to-switch "Switch to buffer in other frame: ")))
  (+amos/workspace-new)
  (switch-to-buffer buffer-or-name norecord))

(defun +amos/workspace-new-scratch ()
  (interactive)
  (+amos/switch-to-buffer-other-frame "*scratch"))

(defun +amos/tmux-fork-window (&optional command)
  "Detach if inside tmux."
  (interactive)
  (+amos-store-jump-history)
  (if command
      (shell-command! (format "tmux switch-client -t amos; tmux run -t amos \"tmux if-shell \\\"tmux new-window -t $envprompt -k -n $envprompt -c %s\\\" '' \\\"new-window -n $envprompt -c %s\\\"; tmux send-keys %s C-m\"" default-directory default-directory command))
    (shell-command! (format "tmux switch-client -t amos; tmux run -t amos \"tmux new-window -c %s\"" default-directory))))

(defun +amos/tmux-source ()
  "Source tmux config if inside tmux."
  (interactive)
  (shell-command! "tmux source-file ~/.tmux/.tmux.conf.emacs"))

(evil-define-operator +amos/evil-commentary-yank-line (beg end type)
  "Saves whole lines into the kill-ring."
  :motion evil-line
  :move-point nil
  (interactive "<R>")
  (let* ((beg (save-excursion (beginning-of-line) (point)))
         (end (save-excursion (end-of-line) (point)))
         (column (evil-column))
         (line (buffer-substring-no-properties beg end)))
    (evil-commentary-line beg end)
    (end-of-line)
    (open-line 1)
    (forward-line 1)
    (insert line)
    (move-to-column column)))

(defun +amos/prompt-kill-emacs ()
  "Prompt to save changed buffers and exit Spacemacs"
  (interactive)
  (save-some-buffers nil t)
  (kill-emacs))

(defun +amos*lsp--xref-make-item (filename range)
  "Return a xref-item from a RANGE in FILENAME."
  (let* ((pos-start (gethash "start" range))
         (pos-end (gethash "end" range))
         (line (lsp--extract-line-from-buffer pos-start))
         (start (gethash "character" pos-start))
         (end (gethash "character" pos-end))
         (len (length line)))
    (add-face-text-property (max (min start len) 0)
                            (max (min end len) 0)
                            'ivy-minibuffer-match-face-2 t line) ;; NOTE change face
    ;; LINE is nil when FILENAME is not being current visited by any buffer.
    (xref-make (or line filename)
               (xref-make-file-location filename
                                        (1+ (gethash "line" pos-start))
                                        (gethash "character" pos-start)))))
(advice-add #'lsp--xref-make-item :override #'+amos*lsp--xref-make-item)

(defun +amos*xref--collect-matches-1 (regexp file line line-beg line-end syntax-needed)
  (let (matches)
    (when syntax-needed
      (syntax-propertize line-end))
    ;; FIXME: This results in several lines with the same
    ;; summary. Solve with composite pattern?
    (while (and
            ;; REGEXP might match an empty string.  Or line.
            (or (null matches)
                (> (point) line-beg))
            (re-search-forward regexp line-end t))
      (let* ((beg-column (- (match-beginning 0) line-beg))
             (end-column (- (match-end 0) line-beg))
             (loc (xref-make-file-location file line beg-column))
             (summary (buffer-substring-no-properties line-beg line-end)))
        (add-face-text-property beg-column end-column 'ivy-minibuffer-match-face-2
                                t summary)
        (push (xref-make-match summary loc (- end-column beg-column))
              matches)))
    (nreverse matches)))
(advice-add #'xref--collect-matches-1 :override #'+amos*xref--collect-matches-1)

(defun comp-buffer-name (maj-mode)
  (concat "*" (downcase maj-mode) " " default-directory "*"))
(setq compilation-buffer-name-function #'comp-buffer-name)
(defun +amos/normalize-compilation-buffer (buffer msg)
  (interactive)
  (with-current-buffer buffer
    (evil-normal-state)))
(add-hook! 'compilation-finish-functions #'+amos/normalize-compilation-buffer)

(defvar +amos-frame-list nil)
(defvar +amos-frame-stack nil)
(defvar +amos-tmux-need-switch nil)

;; vertical bar
(add-hook! 'doom-load-theme-hook
  (set-face-background 'vertical-border "#282c34"))
(add-hook! 'after-make-frame-functions
  (set-face-background 'vertical-border "#282c34")
  (unless +amos-frame-list
    (setq +amos-frame-list (+amos--frame-list-without-daemon))))
(defsubst +amos--is-frame-daemons-frame (f)
  (and (daemonp) (eq f terminal-frame)))

(defun +amos--frame-list-without-daemon ()
  (if (daemonp)
      (filtered-frame-list
       #'(lambda (f) (not (+amos--is-frame-daemons-frame f))))
    (frame-list)))

(defun +amos/workspace-new ()
  (interactive)
  (if gui-p (setenv "DISPLAY" ":0"))
  (let ((name (frame-parameter nil 'name))
        (oframe (selected-frame))
        (nframe))
    (make-frame-invisible oframe t)
    (setq nframe (if (s-starts-with? "F" name)
                     (make-frame)
                   (make-frame `((name . ,name)))))
    (select-frame nframe)
    (let ((dir default-directory))
      (switch-to-buffer "*scratch*")
      (cd dir))
    (push nframe +amos-frame-stack)
    (setq +amos-frame-list
          (-insert-at (1+ (-elem-index oframe +amos-frame-list)) nframe +amos-frame-list))))

(defun +amos/workspace-delete ()
  (interactive)
  (let ((f (selected-frame)))
    (setq +amos-frame-list (--remove (eq f it) +amos-frame-list))
    (setq +amos-frame-stack (-uniq (--remove (eq f it) +amos-frame-stack)))
    (if +amos-frame-stack
        (+amos/workspace-switch-to-frame (car +amos-frame-stack)))
    (delete-frame f))
  (when +amos-tmux-need-switch
    (shell-command! "tmux switch-client -t amos\; run-shell -t amos '/home/amos/scripts/setcursor.sh $(tmux display -p \"#{pane_tty}\")'")
    (setq +amos-tmux-need-switch nil)))

(defun +amos/workspace-switch-to-frame (frame)
  (setq +amos-tmux-need-switch nil)
  (let ((oframe (selected-frame)))
    (make-frame-invisible oframe t)
    (select-frame frame)
    (raise-frame frame)
    (push frame +amos-frame-stack)
    (+amos/reset-cursor)
    (recenter)))

(defun +amos/workspace-switch-to (index)
  (interactive)
  (when (< index (length +amos-frame-list))
    (let ((frame (nth index +amos-frame-list)))
      (+amos/workspace-switch-to-frame frame))))

(defun +amos/workspace-switch-to-1 () (interactive) (+amos/workspace-switch-to 0))
(defun +amos/workspace-switch-to-2 () (interactive) (+amos/workspace-switch-to 1))
(defun +amos/workspace-switch-to-3 () (interactive) (+amos/workspace-switch-to 2))
(defun +amos/workspace-switch-to-4 () (interactive) (+amos/workspace-switch-to 3))
(defun +amos/workspace-switch-to-5 () (interactive) (+amos/workspace-switch-to 4))
(defun +amos/workspace-switch-to-6 () (interactive) (+amos/workspace-switch-to 5))
(defun +amos/workspace-switch-to-7 () (interactive) (+amos/workspace-switch-to 6))
(defun +amos/workspace-switch-to-8 () (interactive) (+amos/workspace-switch-to 7))
(defun +amos/workspace-switch-to-9 () (interactive) (+amos/workspace-switch-to 8))
(defun +amos-workspace-cycle (off)
  (let* ((n (length +amos-frame-list))
         (index (-elem-index (selected-frame) +amos-frame-list))
         (i (% (+ off index n) n)))
    (+amos/workspace-switch-to i)))
(defun +amos/workspace-switch-left ()  (interactive) (+amos-workspace-cycle -1))
(defun +amos/workspace-switch-right () (interactive) (+amos-workspace-cycle +1))

(defun +amos|maybe-delete-frame-buffer (frame)
  (let ((windows (window-list frame)))
    (dolist (window windows)
      (let ((buffer (window-buffer (car windows))))
        (when (eq 1 (length (get-buffer-window-list buffer nil t)))
          (kill-buffer buffer))))))
(add-to-list 'delete-frame-functions #'+amos|maybe-delete-frame-buffer)

(defun +amos*flycheck-next-error-function (n reset)
  (-if-let* ((pos (flycheck-next-error-pos n reset))
             (err (get-char-property pos 'flycheck-error))
             (filename (flycheck-error-filename err))
             (dummy (string= buffer-file-name filename)))
      (flycheck-jump-to-error err)
    (user-error "No more Flycheck errors")))
(advice-add #'flycheck-next-error-function :override #'+amos*flycheck-next-error-function)

;; only reuse current frame's popup
(defadvice +popup-display-buffer (around +amos*popup-display-buffer activate)
  (doom-with-advice (get-window-with-predicate (lambda (orig-fun f &rest _) (funcall orig-fun f)))
      ad-do-it))

(def-package! dired-open
  :after dired
  :config
  (push #'+amos/dired-open-callgrind dired-open-functions))

(def-package! dired-quick-sort
  :after dired
  :config
  (dired-quick-sort-setup))

(after! dired-x
  (setq dired-omit-files
        (concat dired-omit-files "\\|\\.directory$"))
  (add-hook! 'dired-mode-hook
    (let ((inhibit-message t))
      (toggle-truncate-lines +1)
      ;; (dired-omit-mode) ;; .d folders are gone
      (+amos-store-jump-history))))

(define-advice dired-revert (:after (&rest _) +amos*dired-revert)
  "Call `recenter' after `dired-revert'."
  (condition-case nil
      (+amos/recenter)
    (error nil)))

(after! wdired
  (evil-set-initial-state 'wdired-mode 'normal))

(defun +amos/dired-open-callgrind ()
  "Open callgrind files according to its name."
  (interactive)
  (let ((file (ignore-errors (dired-get-file-for-visit)))
        process)
    (when (and file
               (not (file-directory-p file)))
      (when (string-match-p "$[cachegrind|callgrind].out" file)
        (setq process (dired-open--start-process file "kcachegrind"))))
    process))

(defadvice dired-clean-up-after-deletion (around +amos*dired-clean-up-after-deletion activate)
  (doom-with-advice (y-or-n-p (lambda (&rest _) t))
      ad-do-it))

(after! evil-snipe
  (push 'dired-mode evil-snipe-disabled-modes)
  )

;; fix constructor list
(defun +amos*c-determine-limit (orig-fun &rest args)
  (setf (car args) 5000)
  (apply orig-fun args))
(advice-add #'c-determine-limit :around #'+amos*c-determine-limit)

(evil-define-state lisp
  "Lisp state.
 Used to navigate lisp code and manipulate the sexp tree."
  :tag " <L> "
  :cursor (bar . 2)
  ;; force smartparens mode
  (if (evil-lisp-state-p) (smartparens-mode)))

(set-keymap-parent evil-lisp-state-map evil-insert-state-map)

(general-define-key
 :states 'lisp
 "<escape>"       (lambda! (evil-normal-state) (unless (bolp) (backward-char)))
 "M-o"            #'lisp-state-toggle-lisp-state
 "M-U"            #'+amos/replace-defun
 "M-u"            #'eval-defun
 "C-a"            #'sp-beginning-of-sexp
 "C-e"            #'sp-end-of-sexp
 "C-n"            #'sp-down-sexp
 "C-p"            #'sp-up-sexp
 "M-n"            #'sp-backward-down-sexp
 "M-p"            #'sp-backward-up-sexp
 "M-f"            #'sp-forward-sexp
 "M-b"            #'sp-backward-sexp
 "M-,"            #'sp-backward-unwrap-sexp
 "M-."            #'sp-unwrap-sexp
 "M-r"            #'sp-forward-slurp-sexp
 "M-R"            #'sp-forward-barf-sexp
 "M-s"            #'sp-splice-sexp
 "M-t"            #'sp-transpose-sexp
 "C-t"            #'sp-transpose-hybrid-sexp
 "M-d"            #'sp-kill-sexp
 "C-o"            #'sp-kill-hybrid-sexp
 [M-backspace]    #'sp-backward-kill-sexp
 [134217855]      #'sp-backward-kill-sexp ; M-DEL
 "M-w"            #'sp-copy-sexp
 "M-("            #'sp-wrap-round
 "M-{"            #'sp-wrap-curly
 "M-["            #'sp-wrap-square
 "M-\""           (lambda! (sp-wrap-with-pair "\"")))

(defun lisp-state-toggle-lisp-state ()
  "Toggle the lisp state."
  (interactive)
  (if (eq 'lisp evil-state)
      (progn
        (message "state: lisp -> insert")
        (evil-insert-state))
    (message "state: %s -> lisp" evil-state)
    (evil-lisp-state)))

(defun lisp-state-wrap (&optional arg)
  "Wrap a symbol with parenthesis."
  (interactive "P")
  (sp-wrap-with-pair "("))

(defun evil-lisp-state-next-paren (&optional closing)
  "Go to the next/previous closing/opening parenthesis/bracket/brace."
  (if closing
      (let ((curr (point)))
        (forward-char)
        (unless (eq curr (search-forward-regexp "[])}]"))
          (backward-char)))
    (search-backward-regexp "[[({]")))

(defun lisp-state-prev-opening-paren ()
  "Go to the next closing parenthesis."
  (interactive)
  (evil-lisp-state-next-paren))

(defun lisp-state-next-closing-paren ()
  "Go to the next closing parenthesis."
  (interactive)
  (evil-lisp-state-next-paren 'closing))

(defun lisp-state-forward-symbol (&optional arg)
  "Go to the beginning of the next symbol."
  (interactive "P")
  (let ((n (if (char-equal (char-after) ?\() 1 2)))
    (sp-forward-symbol (+ (if arg arg 0) n))
    (sp-backward-symbol)))

(defun lisp-state-insert-sexp-after ()
  "Insert sexp after the current one."
  (interactive)
  (let ((sp-navigate-consider-symbols nil))
    (if (char-equal (char-after) ?\() (forward-char))
    (sp-up-sexp)
    (evil-insert-state)
    (sp-newline)
    (sp-insert-pair "(")))

(defun lisp-state-insert-sexp-before ()
  "Insert sexp before the current one."
  (interactive)
  (let ((sp-navigate-consider-symbols nil))
    (if (char-equal (char-after) ?\() (forward-char))
    (sp-backward-sexp)
    (evil-insert-state)
    (sp-newline)
    (evil-previous-visual-line)
    (evil-end-of-line)
    (insert " ")
    (sp-insert-pair "(")
    (indent-for-tab-command)))

(defun lisp-state-eval-sexp-end-of-line ()
  "Evaluate the last sexp at the end of the current line."
  (interactive)
  (save-excursion
    (end-of-line)
    (eval-last-sexp nil)))

(defun lisp-state-beginning-of-sexp (&optional arg)
  "Go to the beginning of current s-exp"
  (interactive "P")
  (sp-beginning-of-sexp)
  (evil-backward-char))

(def-package! syntactic-close)

(after! iedit
  (add-hook! 'iedit-mode-end-hook (+amos/recenter) (setq iedit-unmatched-lines-invisible nil)))

(after! magit
  (setq
   magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1
   magit-display-buffer-noselect t
   magit-repository-directories '(("~/git" . 2))
   magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  (defun +amos*remove-git-index-lock (&rest _)
    (ignore-errors
      (delete-file ".git/index.lock")))
  (advice-add #'magit-refresh :before #'+amos*remove-git-index-lock))

(after! evil-magit
  (setq evil-magit-use-z-for-folds nil))

(after! org
  (setq org-image-actual-width '(400)))

(after! recentf
  (setq recentf-max-saved-items 10000))

(def-package! quick-peek)

(defun +amos*flycheck-inline-display-errors (ofun &rest candidate)
  (if (or (eq last-command 'flycheck-previous-error)
          (eq last-command 'flycheck-next-error)
          (eq last-input-event 29))
      (apply ofun candidate)))

(def-package! flycheck
  :commands (flycheck-list-errors flycheck-buffer)
  :after-call (doom-enter-buffer-hook after-find-file)
  :config
  ;; Emacs feels snappier without checks on newline
  (setq flycheck-check-syntax-automatically '(save idle-change mode-enabled))
  (after! evil
    (defun +syntax-checkers|flycheck-buffer ()
      "Flycheck buffer on ESC in normal mode."
      (when flycheck-mode
        (ignore-errors (flycheck-buffer))
        nil))
    (add-hook 'doom-escape-hook #'+syntax-checkers|flycheck-buffer t))
  (global-flycheck-inline-mode +1)
  (global-flycheck-mode +1)
  (advice-add #'flycheck-inline-display-errors :around #'+amos*flycheck-inline-display-errors)
  ;; (advice-add #'flycheck-display-error-messages :override #'flycheck-inline-display-errors)
  (setq flycheck-highlighting-mode 'columns
        flycheck-check-syntax-automatically '(save mode-enabled)
        flycheck-indication-mode nil
        ;; flycheck-inline-display-function
        ;; (lambda (msg pos)
        ;;   (let* ((ov (quick-peek-overlay-ensure-at pos))
        ;;          (contents (quick-peek-overlay-contents ov)))
        ;;     (setf (quick-peek-overlay-contents ov)
        ;;           (concat contents (when contents "\n") msg))
        ;;     (quick-peek-update ov)))
        ;; flycheck-inline-clear-function #'quick-peek-hide
        flycheck-display-errors-delay 0)
  (require 'lsp-ui-flycheck))

(after! counsel
  (setq counsel-rg-base-command "rg -S --no-heading --line-number --color never %s ."))

(advice-add #'evil-multiedit--cycle :after #'+amos/recenter)
(advice-add #'evil-multiedit-match-and-next :after #'+amos/recenter)
(advice-add #'edebug-overlay-arrow :after #'realign-windows)

(evil-define-motion +amos/evil-goto-line (count)
  "Go to the first non-blank character of line COUNT.
By default the last line."
  :jump t
  :type line
  (if (null count)
      (with-no-warnings (end-of-buffer))
    (goto-char (point-min))
    (forward-line (1- count)))
  (evil-first-non-blank)
  (+amos/recenter))

(defun +amos/company-abort ()
  (interactive)
  (if company-selection-changed
      (+amos/company-search-abort)
    (company-abort)
    (call-interactively (key-binding (this-command-keys)))))

(defun +amos/company-search-abort ()
  (interactive)
  (if company-selection-changed
      (progn
        (advice-add 'company-call-backend :before-until 'company-tng--supress-post-completion)
        (company-complete-selection)
        (call-interactively (key-binding (this-command-keys))))
    (company-abort)
    (call-interactively (key-binding (this-command-keys)))))

(defun ediff-copy-both-to-C ()
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-c-to-ediff-mode-map () (define-key ediff-mode-map "c" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-c-to-ediff-mode-map)

(defun +amos/close-block ()
  (interactive)
  (evil-with-state 'insert
    (syntactic-close)))

;; TODO useless?
(defun +amos-get-buffer-by-name (name)
  (cl-loop for buffer in (buffer-list)
           if (or (get-file-buffer name)
                  (string= (buffer-name buffer) name))
           collect buffer))

(advice-add #'evil--jumps-savehist-load :override #'ignore)
(defun +amos-clean-evil-jump-list (&optional buffer)
  (let* ((ring (make-ring evil-jumps-max-length))
         (jump-struct (evil--jumps-get-current))
         (idx (evil-jumps-struct-idx jump-struct))
         (target-list (evil-jumps-struct-ring jump-struct))
         (size (ring-length target-list))
         (i 0))
    (when target-list
      (cl-loop for target in (ring-elements target-list)
               do (let* ((marker (car target))
                         (file-name (cadr target)))
                    (if (or (not (markerp marker))
                            (not (marker-buffer marker))
                            (and buffer
                                 (string= file-name (or buffer-file-name (buffer-name buffer)))))
                        (if (<= i idx) (setq idx (- idx 1)))
                      ;; else
                      (ring-insert-at-beginning ring target)
                      (setq i (+ i 1)))))
      (setf (evil-jumps-struct-ring jump-struct) ring)
      (setf (evil-jumps-struct-idx jump-struct) idx))))

(defun +amos/close-current-buffer (&optional wipe kill)
  (interactive)
  (if wipe
      (+amos-clean-evil-jump-list (current-buffer)))
  (or (and kill (kill-current-buffer)) (bury-buffer)))

(defun +amos*undo-tree (orig-fun &rest args)
  (if (and (not defining-kbd-macro)
           (not executing-kbd-macro))
      (apply orig-fun args)
    (message "cannot use undo when recording/executing a macro!")))
(advice-add #'undo-tree-undo :around #'+amos*undo-tree)
(advice-add #'undo-tree-redo :around #'+amos*undo-tree)

(add-hook! 'eval-expression-minibuffer-setup-hook
  (define-key minibuffer-local-map "\C-p" #'previous-line-or-history-element)
  (define-key minibuffer-local-map "\C-n" #'next-line-or-history-element))

(add-hook! 'minibuffer-setup-hook
  (setq-local truncate-lines t)
  (setq-local inhibit-message t))

(setq window-adjust-process-window-size-smallest #'ignore)
(advice-add #'set-process-window-size :override #'ignore)
(advice-add #'evil-escape-mode :override #'ignore)
(advice-add #'dired-k--highlight-by-file-attribyte :override #'ignore)
(advice-add #'recenter-top-bottom :override #'recenter)
(advice-add #'git-gutter:next-hunk :after (lambda (arg) (recenter)))
(advice-add #'magit-blame--update-margin :override #'ignore)

(defun +amos/avy-goto-url()
  "Use avy to go to an URL in the buffer."
  (interactive)
  (require 'avy)
  (avy--generic-jump "https?://" nil 'pre))

(defun +amos/avy-open-url ()
  "Use avy to select an URL in the buffer and open it."
  (interactive)
  (require 'avy)
  (save-excursion
    (if (+amos/avy-goto-url)
        (browse-url-at-point))))

(defun +amos/avy-goto-char-timer (&optional arg)
  "Read one or many consecutive chars and jump to the first one.
The window scope is determined by `avy-all-windows' (ARG negates it)."
  (interactive "P")
  (require 'avy)
  (let (block)
    (when (eq 'block evil-visual-selection)
      (evil-visual-char)
      (setq block t))
    (let ((avy-all-windows (if arg
                               (not avy-all-windows)
                             avy-all-windows)))
      (avy-with avy-goto-char-timer
                (avy--process
                 (avy--read-candidates)
                 (avy--style-fn avy-style))))
    (if block (evil-visual-block))))
;; (evil-define-avy-motion +amos/avy-goto-char-timer inclusive)

(defun lua-busted-fuckups-fix ()
  (save-excursion
    (lua-forward-line-skip-blanks 'back)
    (let* ((current-indentation (current-indentation))
           (line (thing-at-point 'line t))
           (busted-p (s-matches?
                      (rx (+ bol (* space)
                             (or "context" "describe" "it" "setup" "teardown")
                             "("))
                      line)))
      (when busted-p
        (+ current-indentation lua-indent-level)))))
(defun +amos*lua-calculate-indentation-override (old-function &rest arguments)
  (or (lua-busted-fuckups-fix)
      (apply old-function arguments)))
(advice-add #'lua-calculate-indentation-override :around #'+amos*lua-calculate-indentation-override)

(defun lua-find-matching-token-in-line (found-token found-pos token-type &optional direction)
  (let ((line (line-number-at-pos))
        ;; If we are on a middle token, go backwards. If it is a middle-or-open,
        ;; go forwards.
        (search-direction
         (or direction
             (if (or (eq token-type 'open)
                     (eq token-type 'middle-or-open))
                 'forward
               'backward)
             'backward)))
    (save-excursion
      ;; This is required since lua-find-matching-token-word needs point to be
      ;; at the beginning of the keyword.
      (goto-char found-pos)
      (let ((found-match (lua-find-matching-token-word found-token search-direction)))
        (when (and found-match (= line (line-number-at-pos)))
          (point))))))

(defun lua-resolve-token-type (found-token found-pos)
  "Get resolved token type.
 If token type is 'middle-or-open, determine which one it is and
 return it."
  (save-excursion
    (let ((token-type (lua-get-token-type (lua-get-block-token-info found-token))))
      (if (not (eq token-type 'middle-or-open))
          token-type
        (goto-char found-pos)
        (if (not (lua-find-matching-token-word found-token 'backward))
            'open
          'middle)))))

(defun lua-line-indent-impact-current (&optional bound)
  "Calculate how much current line impacts indentation of current line.
 `bound' is set to `line-end-position' by default."
  ;; TODO: Optimization idea: sum all closers and matched-in-line openers until
  ;; an unmatched-in-line opener is met.
  (unless bound
    (setq bound (line-end-position)))
  (save-excursion
    (back-to-indentation)
    ;; Check first token.
    (if (or (lua-comment-or-string-p)
            (not (looking-at lua-indentation-modifier-regexp)))
        0
      (let ((token-type (lua-resolve-token-type (match-string 0) (match-beginning 0))))
        (cond
         ((eq token-type 'middle)
          (- lua-indent-level))
         ((eq token-type 'close)
          ;; Loop over all tokens in line.
          (back-to-indentation)
          (let ((shift 0))
            (while (lua-find-regexp 'forward lua-indentation-modifier-regexp bound)
              (let ((found-token (match-string 0))
                    (found-pos (match-beginning 0)))
                (setq token-type (lua-resolve-token-type found-token found-pos))
                ;; Only if 'close and unmatched.
                (when (and (eq token-type 'close)
                           (not (lua-find-matching-token-in-line found-token found-pos token-type)))
                  (setq shift (- shift lua-indent-level)))))
            (max -4 shift)))
         (t 0))))))

(defun lua-line-indent-impact-next (&optional bound)
  "Calculate how much current line impacts indentation of next line.
 `bound' is set to `line-end-position' by default."
  (unless bound
    (setq bound (line-end-position)))
  (save-excursion
    (back-to-indentation)
    (let ((shift 0)
          (add 0)
          (first-token-type)
          (token-type))
      ;; Shift if first token is 'middle.
      (when (and (not (lua-comment-or-string-p))
                 (looking-at lua-indentation-modifier-regexp)
                 (eq 'middle (setq first-token-type (lua-resolve-token-type (match-string 0) (match-beginning 0)))))
        (setq shift (+ shift lua-indent-level)))
      ;; Loop over all tokens in line.
      (while (lua-find-regexp 'forward lua-indentation-modifier-regexp bound)
        (let ((found-token (match-string 0))
              (found-pos (match-beginning 0)))
          (setq token-type (lua-resolve-token-type found-token found-pos))
          ;; Only if unmatched 'open and unmatched 'close if first token was not
          ;; 'close.
          (unless (lua-find-matching-token-in-line found-token found-pos token-type)
            (cond
             ((eq token-type 'open)
              (setq add (+ add lua-indent-level)))
             ((and (eq token-type 'close) (not (eq first-token-type 'close)))
              (setq add (- add lua-indent-level)))
             (t 0)))))
      (+ shift (min lua-indent-level add)))))

(defun +amos*lua-calculate-indentation (&optional parse-start)
  "Return appropriate indentation for current line as Lua code."
  ;; Algorithm: indentation is computed from current line and previous line.
  ;; Current line:
  ;; -1 on every unmatched closer if first token is a closer
  ;; -1 if first token is 'middle
  ;; Previous line:
  ;; +1 on every unmatched opener
  ;; +1 if first token is a 'middle
  ;; -1 on every unmatched closer if first token is not a closer (if first token
  ;;  is a 'middle, then first unmatched closer is actually closing the middle).
  ;;
  ;; To this we add
  ;; + previous line indentation
  ;; +1 if previous line is not a continuation and current-line is
  ;; -1 if previous line is a continuation and current-line is not
  (save-excursion
    (back-to-indentation)
    (let* ((continuing-p (lua-is-continuing-statement-p))
           (cur-line-begin-pos (line-beginning-position))
           (close-factor (lua-line-indent-impact-current)))

      (if (lua-forward-line-skip-blanks 'back)
          (+ (current-indentation)
             (lua-line-indent-impact-next cur-line-begin-pos)
             close-factor
             ;; Previous line is a continuing statement, but not current.
             (if (and (lua-is-continuing-statement-p) (not continuing-p))
                 (- lua-indent-level)
               0)
             ;; Current line is a continuing statement, but not previous.
             (if (and (not (lua-is-continuing-statement-p)) continuing-p)
                 lua-indent-level
               0))
        ;; If there's no previous line, indentation is 0.
        0))))
(advice-add #'lua-calculate-indentation :override #'+amos*lua-calculate-indentation)

(global-page-break-lines-mode +1)

(defun anzu-multiedit (&optional symbol)
  (interactive (list evil-symbol-word-search))
  (let ((string (evil-find-thing t (if symbol 'symbol 'word))))
    (if (null string)
        (user-error "No word under point")
      (let* ((regex (if nil ;; unbounded
                        (regexp-quote string)
                      (format (if symbol "\\_<%s\\_>" "\\<%s\\>")
                              (regexp-quote string))))
             (search-pattern (evil-ex-make-search-pattern regex)))
        (evil-multiedit-ex-match (point-min) (point-max) nil (car search-pattern))))))

(defun +amos/lsp-ui-imenu nil
  (interactive)
  (setq lsp-ui-imenu--origin (current-buffer))
  (imenu--make-index-alist)
  (let ((list imenu--index-alist))
    (with-current-buffer (get-buffer-create "*lsp-ui-imenu*")
      (let* ((padding (or (and (eq lsp-ui-imenu-kind-position 'top) 1)
                          (--> (-filter 'imenu--subalist-p list)
                               (--map (length (car it)) it)
                               (-max (or it '(1))))))
             (grouped-by-subs (-partition-by 'imenu--subalist-p list))
             (color-index 0)
             buffer-read-only)
        (remove-overlays)
        (erase-buffer)
        (lsp-ui-imenu--put-separator)
        (dolist (group grouped-by-subs)
          (if (imenu--subalist-p (car group))
              (dolist (kind group)
                (-let* (((title . entries) kind))
                  (lsp-ui-imenu--put-kind title padding color-index)
                  (--each-indexed entries
                    (insert (lsp-ui-imenu--make-line title it-index padding it color-index)))
                  (lsp-ui-imenu--put-separator)
                  (setq color-index (1+ color-index))))
            (--each-indexed group
              (insert (lsp-ui-imenu--make-line " " it-index padding it color-index)))
            (lsp-ui-imenu--put-separator)
            (setq color-index (1+ color-index))))
        (lsp-ui-imenu-mode)
        (setq mode-line-format '(:eval (lsp-ui-imenu--win-separator)))
        (goto-char 1)
        (add-hook 'post-command-hook 'lsp-ui-imenu--post-command nil t)))
    (let ((win (display-buffer-in-side-window (get-buffer "*lsp-ui-imenu*") '((side . right))))
          (fit-window-to-buffer-horizontally t))
      (set-window-margins win 1)
      (select-window win)
      (set-window-start win 1)
      (set-window-dedicated-p win t)
      (let ((fit-window-to-buffer-horizontally 'only))
        (fit-window-to-buffer win))
      (window-resize win 20 t))))

(def-package! color-moccur)

(defun +amos/toggle-mc ()
  (interactive)
  (evil-mc-make-cursor-here)
  (evil-mc-pause-cursors))

(defun +amos/wipe-current-buffer ()
  (interactive)
  (+amos/close-current-buffer t))

(defun +amos/kill-current-buffer ()
  (interactive)
  (+amos/close-current-buffer t t))

(defun +amos/switch-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) t)))

(defmacro +amos-evil-ex! (name command)
  `(evil-define-command ,(intern (concat "+amos/" (symbol-name name))) ()
     (evil-ex ,command)))
(+amos-evil-ex! line-substitute "s/")
(+amos-evil-ex! all-substitute "%s/")
(+amos-evil-ex! region-substitute "'<,'>s/")

(defun +amos/counsel-recentf-no-cache ()
  (interactive)
  (require 'recentf)
  (recentf-cleanup)
  (counsel-recentf))

(defun +amos/reset-zoom ()
  (interactive)
  (text-scale-set 0))

(defun +amos/increase-zoom ()
  (interactive)
  (text-scale-increase 0.5))

(defun +amos/decrease-zoom ()
  (interactive)
  (text-scale-decrease 0.5))

(defun +amos/paste-from-gui ()
  (interactive)
  (insert-for-yank (gui-get-primary-selection)))

(defun +amos/dump-evil-jump-list ()
  (interactive)
  (message (format "idx = %d, size = %d"
                   (evil-jumps-struct-idx (evil--jumps-get-current))
                   (ring-length (evil--jumps-get-window-jump-list)))))

(defun +amos/reset-cursor ()
  (interactive)
  (evil-refresh-cursor)
  (realign-windows))

(remove-hook 'find-file-hook #'+file-templates|check)
(defun +amos*find-file (filename &optional wildcards)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename))
           (buffer
            (let ((value (find-file-noselect filename nil nil wildcards)))
              (if (listp value)
                  (mapcar 'pop-to-buffer-same-window (nreverse value))
                (pop-to-buffer-same-window value))
              (+file-templates|check)
              value)))
      (when line-number
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-number))
        (+amos/recenter))
      buffer)))
(advice-add 'find-file :override #'+amos*find-file)

(mapc #'evil-declare-change-repeat
      '(company-complete-mouse
        ;; +amos/maybe-add-end-of-statement
        +amos/company-abort
        amos-company-files
        company-complete-selection
        company-complete-common))

(defalias '+amos/format-buffer #'+format/region-or-buffer)
;; (defun +amos/format-buffer ()
;;   (interactive)
;;   (pcase major-mode
;;     ('c++-mode (clang-format-buffer))
;;     (_
;;      (save-excursion
;;        (indent-region (point-min) (point-max) nil)))))

(mapc #'evil-declare-ignore-repeat
      '(
        +amos/align-repeat-left
        +amos/align-repeat-right
        +amos/all-substitute
        +amos/avy-goto-char-timer
        +amos/avy-goto-url
        +amos/avy-open-url
        +amos/counsel-projectile-switch-project
        +amos/counsel-recentf-no-cache
        +amos/counsel-rg-cur-dir
        +amos/counsel-rg-projectile
        +amos/decrease-zoom
        +amos/dired-jump
        +amos/direnv-reload
        +amos/dump-evil-jump-list
        +amos/exec-shell-command
        +amos/format-buffer
        +amos/increase-zoom
        +amos/kill-current-buffer
        +amos/line-substitute
        +amos/lsp-ui-imenu
        +amos/launch
        +amos/maybe-add-end-of-statement
        +amos/other-window
        +amos/paste-from-gui
        +amos/projectile-find-file-no-cache
        +amos/projectile-find-other-file
        +amos/redisplay-and-recenter
        +amos/region-substitute
        +amos/rename-current-buffer-file
        +amos/replace-defun
        +amos/replace-last-sexp
        +amos/reset-cursor
        +amos/reset-zoom
        +amos/shell-command-replace
        +amos/smart-jumper-backward
        +amos/smart-jumper-forward
        +amos/switch-buffer
        +amos/tmux-detach
        +amos/tmux-fork-window
        +amos/tmux-source
        +amos/toggle-mc
        +amos/wipe-current-buffer
        +amos/workspace-delete
        +amos/workspace-new
        +amos/workspace-new-scratch
        +amos/workspace-switch-left
        +amos/workspace-switch-right
        +amos/workspace-switch-to-1
        +amos/workspace-switch-to-2
        +amos/workspace-switch-to-3
        +amos/workspace-switch-to-4
        +amos/workspace-switch-to-5
        +amos/workspace-switch-to-6
        +amos/workspace-switch-to-7
        +amos/workspace-switch-to-8
        +amos/workspace-switch-to-9
        +amos/yank-buffer-filename
        +amos/yank-buffer-filename-nondir
        +amos/yank-buffer-filename-with-line-position
        +eval/buffer
        +eval/region-and-replace
        +evil:delete-this-file
        company-select-next-or-abort
        company-select-previous
        counsel-dash-at-point
        counsel-find-file
        counsel-grep-or-swiper
        counsel-projectile-rg
        counsel-recentf
        direnv-edit
        doom/sudo-this-file
        doom/toggle-fullscreen
        easy-hugo
        editorconfig-find-current-editorconfig
        eval-defun
        evil-commentary-line
        evil-multiedit-match-all
        execute-extended-command
        find-file
        flycheck-mode
        flycheck-previous-error
        flycheck-next-error
        git-gutter:next-hunk
        git-gutter:previous-hunk
        git-gutter:revert-hunk
        git-timemachine
        highlight-indentation-current-column-mode
        highlight-indentation-mode
        ivy-resume
        magit-blame
        magit-status
        pp-eval-last-sexp
        rainbow-mode
        rotate-text
        save-buffer
        save-buffer-maybe
        switch-to-buffer
        toggle-truncate-lines
        undo-tree-redo
        undo-tree-undo
        vc-revert
        whitespace-mode
        yasdcv-translate-at-point
        zygospore-toggle-delete-other-windows
        ))

;; debugging eldoc
(defun stupid_function (&optional xxxxxxx1 xxxxxxx2 xxxxxxx3 xxxxxxx4 xxxxxxx5 xxxxxxx6 xxxxxxx7 xxxxxxx8 xxxxxxx9 xxxxxxx10 xxxxxxx11 xxxxxxx12 xxxxxxx13 xxxxxxx14 xxxxxxx15 xxxxxxx16 xxxxxxx17 xxxxxxx18 xxxxxxx19 xxxxxxx20 xxxxxxx21 xxxxxxx22 xxxxxxx23 xxxxxxx24 xxxxxxx25 xxxxxxx26 xxxxxxx27 xxxxxxx28 xxxxxxx29 xxxxxxx30 xxxxxxx31 xxxxxxx32 xxxxxxx33 xxxxxxx34 xxxxxxx35 xxxxxxx36 xxxxxxx37 xxxxxxx38 xxxxxxx39))
(stupid_function)

(defun +amos/find-file-at-point ()
  (interactive)
  (-if-let (s (symbol-at-point))
      (let* ((path (symbol-name s))
             (dir (file-name-directory path))
             (name (file-name-nondirectory path))
             (adir (expand-file-name (or dir "./")))
             (_ (while (not (file-directory-p adir))
                  (let ((tmp (substring adir 0 -1)))
                    (setq adir (file-name-directory tmp))
                    (setq name (concat (file-name-nondirectory tmp) "/" name)))))
             (default-directory adir))
        (minibuffer-with-setup-hook
            (lambda ()
              (insert name))
          (ivy-read "Find file: " #'read-file-name-internal
                    :matcher #'counsel--find-file-matcher
                    :action #'counsel-find-file-action
                    :require-match 'confirm-after-completion
                    :history 'file-name-history
                    :keymap counsel-find-file-map
                    :caller 'counsel-find-file)))
    (user-error "No file at point")))

(defun +amos/upload ()
  (interactive)
  (let ((filename (buffer-file-name))
        tmp)
    (unless filename
      (setq filename (make-temp-file "upload"))
      (write-region (point-min) (point-max) filename)
      (setq tmp t))
    (kill-new
     (string-trim-right
      (shell-command-to-string (concat "upload " filename " out"))))
    (if tmp (delete-file filename))))

(defun +amos*ivy--insert-minibuffer (text)
  "Insert TEXT into minibuffer with appropriate cleanup."
  (let ((resize-mini-windows nil)
        (update-fn (ivy-state-update-fn ivy-last))
        (old-mark (marker-position (mark-marker)))
        deactivate-mark)
    (ivy--cleanup)
    (when update-fn
      (funcall update-fn))
    (ivy--insert-prompt)
    ;; Do nothing if while-no-input was aborted.
    (when (stringp text)
      (if ivy-display-function
          (funcall ivy-display-function text)
        (ivy-display-function-fallback text)))
    (unless (frame-root-window-p (minibuffer-window))
      (with-selected-window (minibuffer-window)
        (set-window-text-height nil
                                (+ ivy-height
                                   (if ivy-add-newline-after-prompt
                                       1
                                     0)))))
    ;; prevent region growing due to text remove/add
    (when (region-active-p)
      (set-mark old-mark))))
(advice-add #'ivy--insert-minibuffer :override #'+amos*ivy--insert-minibuffer)
(advice-add #'semantic-mode :around #'doom*shut-up)

(defun my-inhibit-semantic-p ()
  (not (or (equal major-mode 'c-mode) (equal major-mode 'c++-mode))))
(add-to-list 'semantic-default-submodes 'global-semantic-stickyfunc-mode)
(semantic-mode 1)
(with-eval-after-load 'semantic
  (add-to-list 'semantic-inhibit-functions #'my-inhibit-semantic-p))

(defun my-replace (beg end)
  (interactive
   (list (if (use-region-p) evil-visual-beginning (line-beginning-position))
         (if (use-region-p) evil-visual-end (line-end-position))))
  (save-excursion
    (while (and (goto-char beg)
                (re-search-forward "\\[\\([^]]+\\)\\]" end t))
      (replace-match (format "{%s}" (match-string 1))))))

(add-hook! 'comint-mode-hook
  (add-hook! :local 'evil-insert-state-entry-hook
    (setq comint-move-point-for-output 'this)
    (goto-char (point-max)))
  (add-hook! :local 'evil-insert-state-exit-hook
    (setq comint-move-point-for-output nil)))

(remove-hook '+lookup-definition-functions #'+lookup-dumb-jump-backend)
(remove-hook '+lookup-definition-functions #'+lookup-project-search-backend)

(defun +amos/clear-yasnippet ()
  (interactive)
  (let* ((snippet (car (yas-active-snippets)))
         (active-field (overlay-get yas--active-field-overlay 'yas--field))
         (target-field (yas--find-next-field 1 snippet active-field)))
    (while target-field
      (yas--skip-and-clear target-field)
      (setq target-field (yas--find-next-field 1 snippet target-field)))
    (yas-exit-snippet snippet)))

(def-package! flyspell-lazy
  :config
  (add-to-list 'ispell-extra-args "--sug-mode=ultra"))

(def-package! smart-forward)

(def-package! symbol-overlay
  :commands (symbol-overlay-put))

;; unwind flycheck backtrace
;; (defun doom*flycheck-buffer ()
;;   (interactive)
;;   (flycheck-clean-deferred-check)
;;   (if flycheck-mode
;;       (unless (flycheck-running-p)
;;         (run-hooks 'flycheck-before-syntax-check-hook)
;;         (flycheck-clear-errors)
;;         (flycheck-mark-all-overlays-for-deletion)
;;         (let* ((checker (flycheck-get-checker-for-buffer)))
;;           (if checker
;;               (flycheck-start-current-syntax-check checker)
;;             (flycheck-clear)
;;             (flycheck-report-status 'no-checker))))
;;     (user-error "Flycheck mode disabled")))
;; (advice-add #'flycheck-buffer :override #'doom*flycheck-buffer)
;; (defun doom*flycheck-start-command-checker (checker callback)
;;   (let (process)
;;     (let* ((program (flycheck-find-checker-executable checker))
;;            (args (flycheck-checker-substituted-arguments checker))
;;            (command (funcall flycheck-command-wrapper-function
;;                              (cons program args)))
;;            (process-connection-type nil))
;;       (setq process (apply 'start-process (format "flycheck-%s" checker)
;;                            nil command))
;;       (setf (process-sentinel process) #'flycheck-handle-signal)
;;       (setf (process-filter process) #'flycheck-receive-checker-output)
;;       (set-process-query-on-exit-flag process nil)
;;       (process-put process 'flycheck-checker checker)
;;       (process-put process 'flycheck-callback callback)
;;       (process-put process 'flycheck-buffer (current-buffer))
;;       (process-put process 'flycheck-working-directory default-directory)
;;       (process-put process 'flycheck-temporaries flycheck-temporaries)
;;       (setq flycheck-temporaries nil)
;;       (when (flycheck-checker-get checker 'standard-input)
;;         (flycheck-process-send-buffer process))
;;       process)))
;; (advice-add #'flycheck-start-command-checker :override #'doom*flycheck-start-command-checker)

;; (defun +amos*git-link--select-remote ()
;;   (if current-prefix-arg
;;       (git-link--read-remote)
;;     (or (magit-get-upstream-remote) (magit-get-push-remote) "origin")))
;; (setq git-link-default-branch "master")
;; (advice-add #'git-link--select-remote :override #'+amos*git-link--select-remote)

(evil-define-command +amos*evil-scroll-down (count)
  "Scrolls the window and the cursor COUNT lines downwards.
If COUNT is not specified the function scrolls down
`evil-scroll-count', which is the last used count.
If the scroll count is zero the command scrolls half the screen."
  :repeat nil
  :keep-visual t
  (interactive "<c>")
  (evil-save-column
    (setq count (or count (max 0 evil-scroll-count)))
    (setq evil-scroll-count count)
    (when (eobp) (signal 'end-of-buffer nil))
    (when (zerop count)
      (setq count (/ (1- (window-height)) 2)))
    ;; BUG #660: First check whether the eob is visible.
    ;; In that case we do not scroll but merely move point.
    (if (<= (point-max) (window-end))
        (with-no-warnings (next-line count nil))
      (let ((xy (posn-x-y (posn-at-point))))
        (condition-case nil
            (progn
              (scroll-up count)
              (let* ((wend (window-end nil t))
                     (p (posn-at-x-y (car xy) (cdr xy)))
                     ;; amos: header line breaks
                     (p2 (posn-at-x-y (car xy) (1+ (cdr xy))))
                     (margin (max 0 (- scroll-margin
                                       (cdr (posn-col-row p))))))
                (goto-char (or (posn-point p) (posn-point p2)))
                ;; ensure point is not within the scroll-margin
                (when (> margin 0)
                  (with-no-warnings (next-line margin))
                  (recenter scroll-margin))
                (when (<= (point-max) wend)
                  (save-excursion
                    (goto-char (point-max))
                    (recenter (- (max 1 scroll-margin)))))))
          (end-of-buffer
           (goto-char (point-max))
           (recenter (- (max 1 scroll-margin)))))))))
(advice-add #'evil-scroll-down :override #'+amos*evil-scroll-down)

(defun +amos*evil-insert-newline-below ()
  "Inserts a new line below point and places point in that line
with regard to indentation."
  (evil-narrow-to-field
    (evil-move-end-of-line)
    (if (not (looking-at "\n"))
        (insert (if use-hard-newlines hard-newline "\n"))
      (forward-char 1)
      (insert (if use-hard-newlines hard-newline "\n"))
      (backward-char 1))
    (back-to-indentation)))
(advice-add #'evil-insert-newline-below :override #'+amos*evil-insert-newline-below)

(evil-define-motion +amos*evil-ret (count)
  "Move the cursor COUNT lines down.
If point is on a widget or a button, click on it.
In Insert state, insert a newline."
  :type line
  (let ((nl (looking-at "\n")))
    (if nl (forward-char 1))
    (evil-ret-gen count nil)
    (if nl (backward-char 1))))
(advice-add #'evil-ret :override #'+amos*evil-ret)

(defun +amos*git-gutter:search-near-diff-index (diffinfos is-reverse)
  (let ((lines (--map (git-gutter-hunk-start-line it) diffinfos)))
    (if is-reverse
        (--find-last-index (> (line-number-at-pos) it) lines)
      (--find-index (< (line-number-at-pos) it) lines))))
(advice-add #'git-gutter:search-near-diff-index :override #'+amos*git-gutter:search-near-diff-index)

(defun +amos-company--insert-candidate (candidate)
  (when (> (length candidate) 0)
    (setq candidate (substring-no-properties candidate))
    (let* ((prefix (s-shared-start company-prefix candidate))
           (non-prefix (substring company-prefix (length prefix))))
      (delete-region (- (point) (length non-prefix)) (point))
      (insert (substring candidate (length prefix))))))
(advice-add #'company--insert-candidate :override #'+amos-company--insert-candidate)

;; company

(setq-default company-idle-delay 0.3
              company-auto-complete nil
              company-tooltip-limit 14
              company-dabbrev-downcase nil
              company-dabbrev-ignore-case nil
              company-dabbrev-code-other-buffers t
              company-dabbrev-code-time-limit 0.3
              company-dabbrev-ignore-buffers "\\`[ *]"
              company-tooltip-align-annotations t
              company-require-match 'never
              company-global-modes '(not eshell-mode comint-mode erc-mode message-mode help-mode gud-mode)
              company-frontends (append '(company-tng-frontend) company-frontends)
              company-backends '(company-lsp company-capf company-dabbrev company-ispell company-yasnippet)
              company-transformers nil
              company-lsp-async t
              company-lsp-cache-candidates nil
              company-search-regexp-function 'company-search-flex-regexp)
(defvar-local company-fci-mode-on-p nil)
(defun company-turn-off-fci (&rest ignore)
  (when (boundp 'fci-mode)
    (setq company-fci-mode-on-p fci-mode)
    (when fci-mode (fci-mode -1))))
(defun company-maybe-turn-on-fci (&rest ignore)
  (when company-fci-mode-on-p (fci-mode 1)))
(add-hook 'company-completion-started-hook   #'company-turn-off-fci)
(add-hook 'company-completion-finished-hook  #'company-maybe-turn-on-fci)
(add-hook 'company-completion-cancelled-hook #'company-maybe-turn-on-fci)

(defvar-local company-tng--overlay nil)

(defun company-tng-frontend (command)
  (cl-case command
    (show
     (let ((ov (make-overlay (point) (point))))
       (setq company-tng--overlay ov)
       (overlay-put ov 'priority 2))
     (advice-add 'company-select-next :before-until 'company-tng--allow-unselected)
     (advice-add 'company-fill-propertize :filter-args 'company-tng--adjust-tooltip-highlight))
    (update
     (let* ((ov company-tng--overlay)
            (candidate (nth company-selection company-candidates))
            (prefix (s-shared-start company-prefix candidate))
            (non-prefix (substring company-prefix (length prefix)))
            (selected (substring candidate (length prefix)))
            (selected (and company-selection-changed
                           (if (iedit-find-current-occurrence-overlay)
                               (propertize selected 'face 'iedit-occurrence)
                             selected))))
       (move-overlay ov (- (point) (length non-prefix)) (point))
       (overlay-put ov (if (= (length non-prefix) 0) 'after-string 'display)
                    selected)))
    (hide
     (when company-tng--overlay
       (delete-overlay company-tng--overlay)
       (kill-local-variable 'company-tng--overlay))
     (advice-remove 'company-select-next 'company-tng--allow-unselected)
     (advice-remove 'company-fill-propertize 'company-tng--adjust-tooltip-highlight))
    (pre-command
     (when (and company-selection-changed
                (not (company--company-command-p (this-command-keys))))
       (company--unread-this-command-keys)
       (setq this-command 'company-complete-selection)
       (advice-add 'company-call-backend :before-until 'company-tng--supress-post-completion)))))

(defun company-tng--allow-unselected (&optional arg)
  "Advice `company-select-next' to allow for an 'unselected'
state. Unselected means that no user interaction took place on the
completion candidates and it's marked by setting
`company-selection-changed' to nil. This advice will call the underlying
`company-select-next' unless we need to transition to or from an unselected
state.

Possible state transitions:
- (arg > 0) unselected -> first candidate selected
- (arg < 0) first candidate selected -> unselected
- (arg < 0 wrap-round) unselected -> last candidate selected
- (arg < 0 no wrap-round) unselected -> unselected

There is no need to advice `company-select-previous' because it calls
`company-select-next' internally."
  (cond
   ;; Selecting next
   ((or (not arg) (> arg 0))
    (unless company-selection-changed
      (company-set-selection (1- (or arg 1)) 'force-update)
      t))
   ;; Selecting previous
   ((< arg 0)
    (when (and company-selection-changed
               (< (+ company-selection arg) 0))
      (company-set-selection 0)
      (setq company-selection-changed nil)
      (company-call-frontends 'update)
      t)
    )))

(defun company-tng--adjust-tooltip-highlight (args)
  (unless company-selection-changed
    (setf (nth 3 args) nil))
  args)

(defun company-tng--supress-post-completion (command &rest args)
  (when (eq command 'post-completion)
    (advice-remove 'company-call-backend 'company-tng--supress-post-completion)
    t))

(defun +amos*doom-buffer-frame-predicate (buf)
  (let ((mode (with-current-buffer buf major-mode)))
    (pcase mode
      ('dired-mode nil)
      (_ t))))
(advice-add #'doom-buffer-frame-predicate :override #'+amos*doom-buffer-frame-predicate)

(defun +amos-display-buffer-no-reuse-window (&rest _) nil)

(defun +amos/exec-shell-command ()
  (interactive)
  (ivy-read "Shell command: " (split-string (shell-command-to-string "bash -c 'compgen -c' | tail -n +85") "\n")
            :action #'compile
            :caller '+amos-exec-shell-command))

;; get rid of minibuffer resize limitation
(defun +amos*window--resize-mini-window (window delta)
  "Resize minibuffer window WINDOW by DELTA pixels.
If WINDOW cannot be resized by DELTA pixels make it as large (or
as small) as possible, but don't signal an error."
  (when (window-minibuffer-p window)
    (let* ((frame (window-frame window))
	       (root (frame-root-window frame))
	       (height (window-pixel-height window))
	       (min-delta
	        (- (window-pixel-height root)
	           (window-min-size root nil t t)))) ;; amos
      ;; Sanitize DELTA.
      (cond
       ((<= (+ height delta) 0)
	    (setq delta (- (frame-char-height (window-frame window)) height)))
       ((> delta min-delta)
	    (setq delta min-delta)))

      (unless (zerop delta)
	    ;; Resize now.
	    (window--resize-reset frame)
	    ;; Ideally we should be able to resize just the last child of root
	    ;; here.  See the comment in `resize-root-window-vertically' for
	    ;; why we do not do that.
	    (window--resize-this-window root (- delta) nil nil t)
	    (set-window-new-pixel window (+ height delta))
	    ;; The following routine catches the case where we want to resize
	    ;; a minibuffer-only frame.
	    (when (resize-mini-window-internal window)
	      (window--pixel-to-total frame)
	      (run-window-configuration-change-hook frame))))))
(advice-add #'window--resize-mini-window :override #'+amos*window--resize-mini-window)

(defun +amos/swiper ()
  (interactive)
  (if (eq major-mode 'ivy-occur-grep-mode)
      (save-restriction
        (save-excursion
          (goto-char 1)
          (forward-line 4)
          (narrow-to-region (point) (point-max)))
        (swiper))
    (swiper)))

(defun +amos/wgrep-occur ()
  "Invoke the search+replace wgrep buffer on the current ag/rg search results."
  (interactive)
  (unless (window-minibuffer-p)
    (user-error "No completion session is active"))
  (require 'wgrep)
  (let* ((ob (ivy-state-buffer ivy-last))
         (caller (ivy-state-caller ivy-last))
         (xref (eq caller '+lookup/references))
         (recursive (and (eq (with-current-buffer ob major-mode) 'ivy-occur-grep-mode)
                         (eq caller 'swiper)))
         (occur-fn (plist-get ivy--occurs-list caller))
         (buffer (generate-new-buffer
                  (format "*ivy-occur%s \"%s\"*"
                          (if caller (concat " " (prin1-to-string caller)) "")
                          ivy-text))))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)
        (if recursive (+amos/swiper-occur)
          (if (not xref)
              (funcall occur-fn)
            (ivy-occur-grep-mode)
            ;; Need precise number of header lines for `wgrep' to work.
            (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                            default-directory))
            (insert (format "%d candidates:\n" (length +amos-last-xref-list)))
            (ivy--occur-insert-lines +amos-last-xref-list))))
      (setf (ivy-state-text ivy-last) ivy-text)
      (setq ivy-occur-last ivy-last)
      (setq-local ivy--directory ivy--directory))
    (ivy-exit-with-action
     `(lambda (_)
        (if ,recursive
            (progn
              (switch-to-buffer ,buffer)
              (kill-buffer ,ob))
          (pop-to-buffer ,buffer))
        (ivy-wgrep-change-to-wgrep-mode)))))

(defun +amos/swiper-occur (&optional revert)
  "Generate a custom occur buffer for `swiper'.
When REVERT is non-nil, regenerate the current *ivy-occur* buffer.
When capture groups are present in the input, print them instead of lines."
  (let* ((buffer (ivy-state-buffer ivy-last))
         (re (progn (string-match "\"\\(.*\\)\"" (buffer-name))
                    (match-string 1 (buffer-name))))
         (re (mapconcat #'identity (ivy--split re) ".*?"))
         (cands
          (mapcar
           (lambda (s)
             (let* ((n (get-text-property 0 'swiper-line-number s))
                    (i (string-match-p "[ \t\n\r]+\\'" n)))
               (when i (setq n (substring n 0 i)))
               (put-text-property 0 (length n) 'face 'compilation-line-number n)
               (format "%s" (substring s 1))))
           (if (not revert)
               ivy--old-cands
             (setq ivy--old-re nil)
             (let ((ivy--regex-function 'swiper--re-builder))
               (ivy--filter re (with-current-buffer buffer
                                 (swiper--candidates))))))))
    (if (string-match-p "\\\\(" re)
        (insert
         (mapconcat #'identity
                    (swiper--extract-matches
                     re (with-current-buffer buffer
                          (swiper--candidates)))
                    "\n"))
      (unless (eq major-mode 'ivy-occur-grep-mode)
        (ivy-occur-grep-mode))
      (setq swiper--current-window-start nil)
      (insert (format "-*- mode:grep; default-directory: %S -*-\n\n\n"
                      default-directory))
      (insert (format "%d candidates:\n" (length cands)))
      (ivy--occur-insert-lines cands)
      (ivy-wgrep-change-to-wgrep-mode)
      (goto-char (point-min))
      (forward-line 4))))

(defun +amos/delete-nonascii (beg end)
  "Delete binary characters in a region"
  (interactive "r")
  (save-excursion
    (save-restriction
      (narrow-to-region beg end)
      (goto-char (point-min))
      (while (re-search-forward "[^[:ascii:]]" nil t)
        (replace-match "")))))

(evil-define-command +amos/fish-editor (file &optional pos)
  (find-file file)
  (if pos
      (goto-char pos))
  (fish-mode)
  (setq buffer-offer-save nil)
  (setq mode-line-format nil)
  (general-define-key
   :states '(normal insert)
   :keymaps 'local
   "C-c C-c" (lambda! (save-buffer) (+amos/kill-current-buffer) (delete-frame))
   "C-c C-k" (lambda! (delete-region (point-min) (point-max)) (save-buffer) (+amos/kill-current-buffer) (delete-frame))))

(defun +amos/launch ()
  (interactive)
  (let ((default-directory (doom-project-root)))
    (+amos/tmux-fork-window "launch.sh")))

(defun +amos/list-file (&optional initial-input)
  (interactive)
  (ivy-read "List file: " (split-string (shell-command-to-string "find -- * -prune -type f -print && find .* -prune -type f -print") "\n")
            :initial-input initial-input
            :action #'find-file
            :preselect (counsel--preselect-file)
            :require-match 'confirm-after-completion
            :history 'file-name-history
            :keymap counsel-find-file-map
            :caller '+amos/list-file))

(defun +amos/iedit-number-occurrences ()
  (interactive)
  (-when-let* ((overlay (iedit-find-current-occurrence-overlay))
               (delta (- (point) (overlay-start overlay))))
    (iedit-barf-if-buffering)
    (setq format-string "%d")
    (let ((iedit-number-occurrence-counter 1)
          (inhibit-modification-hooks t))
      (save-excursion
        (goto-char (+ delta (iedit-first-occurrence)))
        (setq overlay (iedit-find-current-occurrence-overlay))
        (while (/= (point) (point-max))
          (insert (format format-string iedit-number-occurrence-counter))
          (iedit-move-conjoined-overlays overlay)
          (setq iedit-number-occurrence-counter
                (1+ iedit-number-occurrence-counter))
          (goto-char (next-single-char-property-change (point) 'iedit-occurrence-overlay-name))
          (goto-char (next-single-char-property-change (point) 'iedit-occurrence-overlay-name))
          (setq overlay (iedit-find-current-occurrence-overlay))
          (unless (and overlay
                       (= (point) (overlay-end overlay)))
            (goto-char (+ delta (point)))))))))

(defun +amos/shell-command-on-buffer (command)
  (interactive "sShell command on buffer: ")
  (shell-command-on-region (point-min) (point-max) command nil t))

(defun +amos/shell-command-on-region (start end command)
  (interactive (let (string)
                 (unless (mark)
                   (user-error "The mark is not set now, so there is no region"))
                 (setq string (read-shell-command "Shell command on region: "))
                 (list (region-beginning) (region-end) string)))
  (shell-command-on-region start end command nil t))

(defun +amos/revert-projectile-buffers ()
  "Refresh all open file buffers in current projectile without confirmation.
Buffers in modified (not yet saved) state in emacs will not be reverted. They
will be reverted though if they were modified outside emacs.
Buffers visiting files which do not exist any more or are no longer readable
will be killed."
  (interactive)
  (dolist (buf (projectile-project-buffers))
    (let ((filename (buffer-file-name buf)))
      ;; Revert only buffers containing files, which are not modified;
      ;; do not try to revert non-file buffers like *Messages*.
      (when (and filename
                 (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            ;; If the file exists and is readable, revert the buffer.
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          ;; Otherwise, kill the buffer.
          (let (kill-buffer-query-functions) ; No query done when killing buffer
            (kill-buffer buf)
            (message "Killed non-existing/unreadable file buffer: %s" filename))))))
  (message "Finished reverting buffers containing unmodified files."))

(advice-add #'flycheck-previous-error :after (lambda (&rest _) (recenter)))
(advice-add #'flycheck-next-error :after (lambda (&rest _) (recenter)))
