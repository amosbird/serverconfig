;;; private/amos/config.el -*- lexical-binding: t; -*-

(load! "+bindings")

(defun +amos/recenter (&rest _)
  (interactive)
  (recenter)
  (+nav-flash/blink-cursor))

(defvar +amos-dir (file-name-directory load-file-name))
(defvar +amos-snippets-dir (expand-file-name "snippets/" +amos-dir))

(setq epa-file-encrypt-to user-mail-address
      c-tab-always-indent t
      auth-sources (list (expand-file-name ".authinfo.gpg" +amos-dir)))

(defun +amos*no-authinfo-for-tramp (orig-fn &rest args)
  "Don't look into .authinfo for local sudo TRAMP buffers."
  (let ((auth-sources (if (equal tramp-current-method "sudo") nil auth-sources)))
    (apply orig-fn args)))
(advice-add #'tramp-read-passwd :around #'+amos*no-authinfo-for-tramp)

(defun special-indent-fn (pos state)
  (save-excursion
    (search-backward ":hint")
    (current-column)))
(put :hint 'lisp-indent-function 'special-indent-fn)
(put :color 'lisp-indent-function 'defun)
(put :pre 'lisp-indent-function 'defun)
(put :post 'lisp-indent-function 'defun)

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

;; Don't use default snippets, use mine.
(after! yasnippet
  (add-hook! 'yas-minor-mode-hook (yas-activate-extra-mode 'fundamental-mode))
  (setq yas-snippet-dirs
        (append (list '+amos-snippets-dir '+file-templates-dir)
                (delq 'yas-installed-snippets-dir yas-snippet-dirs))))

(after! cus-edit (evil-set-initial-state 'Custom-mode 'normal))
(after! ivy (evil-set-initial-state 'ivy-occur-grep-mode 'normal))
(after! compile (evil-set-initial-state 'compilation-mode 'normal))

(defhydra +amos@paste (:hint nil)
  "Paste"
  ("0" evil-digit-argument-or-evil-beginning-of-line "bol" :exit t)
  ("C-j" evil-paste-pop "Next Paste")
  ("C-k" evil-paste-pop-next "Prev Paste")
  ("p" evil-paste-after "Paste After")
  ("P" evil-paste-before "Paste Before"))

(defun +amos|init-frame (&optional frame)
  (when (and frame (display-graphic-p frame))
    (with-selected-frame frame
      (dolist (charset '(kana han cjk-misc bopomofo))
        (set-fontset-font t charset
                          (font-spec :family "WenQuanYi Micro Hei" :size 18)))
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
    (let* ((buffer (window-buffer window))
           (buffname (string-trim (buffer-name buffer))))
      (or (equal buffname "*doom*")
          (equal buffname "*flycheck-posframe-buffer*")
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

(when (and (not (getenv "TMUX")) (string= (getenv "GUI") "t"))
  (require 'fcitx)
  (fcitx-aggressive-setup))

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
    (let* ((gutter-sep (concat " " (make-string (- (car (window-margins (get-buffer-window))) 2) ? ) sign))
           (face (pcase sign
                   ("=" '+amos:modified)
                   ("+" '+amos:added)
                   ("-" '+amos:deleted)))
           (ovstring (propertize gutter-sep 'face face)))
      (propertize " " 'display `((margin left-margin) ,ovstring))))
  (advice-add #'git-gutter:before-string :override #'+amos*git-gutter:before-string)
  (add-hook! 'window-configuration-change-hook #'git-gutter:update-all-windows))

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


(def-package! link-hint
  :commands link-hint-open-link link-hint-open-all-links
  :config
  (after! mu4e
    (defun +amos/mu4e-open-all-attachments ()
      "Open all visible mu4e attachments."
      (interactive)
      (let ((link-hint-ignore-types
             (remove 'mu4e-attachment link-hint-all-types))
            link-hint-act-on-all-ignore-types)
        (link-hint-open-all-links)))))

(def-package! lispyville
  :commands lispyville-mode)

(def-package! move-text
  :commands move-text-up move-text-down)

(def-package! fish-mode
  :mode "\\.fish")

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

(def-package! rainbow-blocks
  :commands rainbow-blocks-mode)

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
         (+amos/recenter)
         (setf (window-parameter window 'my-last-buffer) new-buffer))))))
(add-hook! 'window-configuration-change-hook #'+amos|update-window-buffer-list)

(autoload #'counsel-projectile-rg "counsel-projectile.el")
(defun +amos/counsel-rg-cur-dir ()
  (interactive)
  (require 'counsel)
  (counsel-rg nil default-directory))

(def-package! yapfify
  :after python)

(def-package! helm-make
  :after ivy
  :config
  (setq helm-make-completion-method 'ivy))

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

(def-package! magit-svn
  :commands turn-on-magit-svn
  :init (add-hook 'magit-mode-hook 'turn-on-magit-svn))

(def-package! page-break-lines
  :commands global-page-break-lines-mode
  :init
  (global-page-break-lines-mode +1))

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
  (let ((files (and projectile-enable-caching
                    (gethash (projectile-project-root) projectile-projects-cache))))
    ;; nothing is cached
    (unless files
      (when projectile-enable-caching
        (message "Empty cache. Projectile is initializing cache..."))
      (setq files
            (projectile-adjust-files
             (split-string
              (shell-command-to-string
               (concat "cd " (projectile-project-root) "; fd --hidden")))))
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
(add-hook! 'projectile-mode-hook (ad-deactivate 'delete-file))

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
  :commands cc-playground cc-playground-mode cc-playground-find-snippet
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
  (company-complete))

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
  (mkr! (kill-region (point) (point-at-eol))))

(evil-define-command +amos/backward-kill-to-bol-and-indent ()
  (let* ((x (save-excursion (doom/backward-to-bol-or-indent)))
         (y (point))
         (a (min x y))
         (b (max x y)))
    (mkr! (kill-region a b))))

(defun +amos-insert-state-p ()
  (require 'evil-multiedit)
  (or (evil-insert-state-p) (evil-multiedit-insert-state-p) (active-minibuffer-window)))

(defun +amos-insert-state ()
  (require 'evil-multiedit)
  (if (evil-multiedit-state-p)
      (evil-multiedit-insert-state)
    (evil-insert-state)))

(defmacro +amos-subword-move! (type command)
  `(evil-define-motion ,(intern (concat "+amos/" (s-replace "word" "subword" (symbol-name command)))) (count &optional bigword)
     :type ,type
     (subword-mode +1)
     (,command count bigword)
     (subword-mode -1)))

(+amos-subword-move! inclusive evil-forward-word-end)
(+amos-subword-move! exclusive evil-forward-word-begin)
(+amos-subword-move! exclusive evil-backward-word-begin)

(evil-define-command +amos/forward-delete-word (&optional subword)
  (evil-signal-at-bob-or-eob 1)
  (unless (+amos-insert-state-p)
    (+amos-insert-state))
  (if subword (subword-mode +1))
  (mkr! (kill-region (point)
                     (max
                      (save-excursion
                        (if (looking-at "[ \t\r\n\v\f]")
                            (progn
                              (re-search-forward "[^ \t\r\n\v\f]")
                              (backward-char))
                          (forward-thing 'evil-word 1))
                        (point))
                      (line-beginning-position))))
  (if subword (subword-mode -1)))
(evil-define-command +amos/forward-delete-subword ()
  (+amos/forward-delete-word t))

(evil-define-command +amos/backward-delete-word (&optional subword)
  (evil-signal-at-bob-or-eob -1)
  (unless (or (eolp) (+amos-insert-state-p))
    (+amos-insert-state)
    (forward-char))
  (if subword (subword-mode +1))
  (mkr! (kill-region (point)
                     (min
                      (save-excursion
                        (if (looking-back "[ \t\r\n\v\f]")
                            (progn
                              (re-search-backward "[^ \t\r\n\v\f]")
                              (forward-char))
                          (forward-thing 'evil-word -1))
                        (point))
                      (line-end-position))))
  (if subword (subword-mode -1)))
(evil-define-command +amos/backward-delete-subword ()
  (+amos/backward-delete-word t))

(evil-define-command +amos/backward-word-insert (&optional subword)
  (evil-signal-at-bob-or-eob -1)
  (if subword (subword-mode +1))
  (unless (or (eolp) (+amos-insert-state-p))
    (+amos-insert-state)
    (forward-char))
  (if (looking-back "[ \t\r\n\v\f]")
      (progn
        (re-search-backward "[^ \t\r\n\v\f]")
        (forward-char))
    (forward-thing 'evil-word -1))
  (if subword (subword-mode -1)))
(evil-define-command +amos/backward-subword-insert ()
  (+amos/backward-word-insert t))

(evil-define-command +amos/forward-word-insert (&optional subword)
  (evil-signal-at-bob-or-eob 1)
  (if subword (subword-mode +1))
  (unless (+amos-insert-state-p)
    (+amos-insert-state))
  (if (looking-at "[ \t\r\n\v\f]")
      (progn
        (re-search-forward "[^ \t\r\n\v\f]")
        (backward-char))
    (forward-thing 'evil-word 1))
  (if subword (subword-mode -1)))
(evil-define-command +amos/forward-subword-insert ()
  (+amos/forward-word-insert t))

(defun +amos*subword-forward-internal ()
  (if superword-mode
      (forward-symbol 1)
    (if (and
         (save-excursion
           (let ((case-fold-search nil))
             (with-syntax-table (make-syntax-table (syntax-table))
               (modify-syntax-entry ?_ "_") ;; added
               (re-search-forward subword-forward-regexp nil t))))
         (> (match-end 0) (point)))
        (goto-char
         (cond
          ((and (< 1 (- (match-end 2) (match-beginning 2)))
                ;; If we have an all-caps word with no following lower-case or
                ;; non-word letter, don't leave the last char (bug#13758).
                (not (and (null (match-beginning 3))
                          (eq (match-end 2) (match-end 1)))))
           (1- (match-end 2)))
          (t
           (match-end 0))))
      (forward-word 1))))
(advice-add #'subword-forward-internal :override #'+amos*subword-forward-internal)

(defun +amos*subword-backward-internal ()
  (if superword-mode
      (forward-symbol -1)
    (if (save-excursion
          (let ((case-fold-search nil))
            (with-syntax-table (make-syntax-table (syntax-table))
              (modify-syntax-entry ?_ "_") ;;  added
              (re-search-backward subword-backward-regexp nil t))))
        (goto-char
         (cond
          ((and (match-end 3)
                (< 1 (- (match-end 3) (match-beginning 3)))
                (not (eq (point) (match-end 3))))
           (1- (match-end 3)))
          (t
           (1+ (match-beginning 0)))))
      (backward-word 1))))
(advice-add #'subword-backward-internal :override #'+amos*subword-backward-internal)

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
  (setq lsp-enable-eldoc nil
        lsp-enable-indentation nil)
  :config
  (require 'lsp-imenu)
  (add-hook 'lsp-after-open-hook #'lsp-enable-imenu))

(def-package! lsp-ui)

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
    (save-buffer)))
(add-hook 'doom-escape-hook #'save-buffer-maybe)

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

(unless (string= (getenv "GUI") "t")
  (advice-add #'switch-to-buffer-other-frame :override #'+amos/switch-to-buffer-other-frame))

(defun +amos/ivy-xref-make-collection (xrefs)
  "Transform XREFS into a collection for display via `ivy-read'."
  (let ((collection nil))
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
          (push `(,candidate . ,location) collection))))
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
          (success nil))
      (ivy-read (concat "Find " (symbol-name kind) ":") (+amos/ivy-xref-make-collection xrefs)
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

(after! ivy

  ;;;###autoload
  (defun amos-recentf ()
    "Find a file on `recentf-list'."
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

  (defun amos-recentf-sort-function (a b)
    (let ((project-root (doom-project-root)))
      (or (file-in-directory-p a project-root) (not (file-in-directory-p b project-root)))))

  (add-to-list 'ivy-sort-functions-alist '(counsel-recentf . amos-recentf-sort-function))


  (dolist (cmd '(counsel-find-file +amos/counsel-projectile-switch-project))
    (ivy-add-actions
     cmd
     '(("f" find-file-other-frame "other frame"))))
  (dolist (cmd '(ivy-switch-buffer))
    (ivy-add-actions
     cmd
     '(("f" switch-to-buffer-other-frame "other frame")))))

;; vertical bar
(add-hook! 'doom-load-theme-hook
  (set-face-background 'vertical-border "#282c34"))
(add-hook! 'after-make-frame-functions
  (set-face-background 'vertical-border "#282c34")
  (setq +amos--frame-list (reverse (+amos--frame-list-without-daemon))))

(defun +amos/redisplay-and-recenter ()
  (interactive)
  (redraw-display)
  (+amos/recenter))

(defun +amos/evil-find-file-at-point-with-line ()
  (interactive)
  (let ((fname (with-no-warnings (ffap-file-at-point))))
    (if fname
        (let ((line
               (save-excursion
                 (goto-char (cadr ffap-string-at-point-region))
                 (and (re-search-backward ":\\([0-9]+\\)\\=" (line-beginning-position) t)
                      (string-to-number (match-string 1))))))
          (with-no-warnings (ffap))
          (when line
            (goto-char (point-min))
            (forward-line (1- line))))
      (user-error "File does not exist."))))

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

(def-package! direnv
  :config
  (direnv-mode))
(add-hook! 'after-save-hook (if (string= (file-name-nondirectory buffer-file-name) ".envrc") (direnv-update-environment)))

(defun +amos/direnv-reload ()
  (interactive)
  (shell-command! "direnv allow")
  (direnv-update-environment)
  (kill-buffer " *direnv*")
  (direnv-mode +1))

(defvar +amos-end-of-statement-regex nil)
(def-setting! :eos (modes &rest plist)
  `(dolist (mode (doom-enlist ,modes))
     (push (cons mode (list ,@plist)) +amos-end-of-statement-regex)))
(set! :eos '(c-mode c++-mode) :regex-char '("[ \t\r\n\v\f]" "[[{(;]" ?\;))
(set! :eos '(emacs-lisp-mode) :regex-char "[ \t\r\n\v\f]")

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

(defvar +file-templates-dir
  (expand-file-name "templates/" +amos-dir)
  "The path to a directory of yasnippet folders to use for file templates.")

(def-package! autoinsert ; built-in
  :commands (auto-insert-mode auto-insert)
  :init
  (setq auto-insert-query nil  ; Don't prompt before insertion
        auto-insert-alist nil) ; Tabula rasa

  ;; load autoinsert as late as possible
  (defun +file-templates|init ()
    (and (not buffer-read-only)
         (bobp) (eobp)
         (remove-hook 'find-file-hook #'+file-templates|init)
         (auto-insert)))
  (add-hook 'find-file-hook #'+file-templates|init)

  :config
  (auto-insert-mode 1)

  (defun +file-templates--expand (key &optional mode project-only)
    "Auto insert a yasnippet snippet into the blank file."
    (when (if project-only (doom-project-p) t)
      (require 'yasnippet)
      (unless yas-minor-mode
        (yas-minor-mode-on))
      (when (and yas-minor-mode
                 (yas-expand-snippet
                  (yas--template-content
                   (cl-find key (yas--all-templates (yas--get-snippet-tables mode))
                            :key #'yas--template-key :test #'equal)))
                 (and (featurep 'evil) evil-mode)
                 (and yas--active-field-overlay
                      (overlay-buffer yas--active-field-overlay)
                      (overlay-get yas--active-field-overlay 'yas--field)))
        (evil-initialize-state 'insert))))

  (defun +file-templates-add (args)
    (cl-destructuring-bind (regexp trigger &optional mode project-only-p) args
      (push `(,regexp . (lambda () (+file-templates--expand ,trigger ',mode ,project-only-p)))
            auto-insert-alist)))

  (mapc #'+file-templates-add
        (let ((doom (concat "/" (regexp-opt '(".emacs.d" ".doom.d" "doom-emacs" ".config/doom")) "/")))
          `(;; General
            ("/\\.gitignore$"                 "__"               gitignore-mode)
            ("/Dockerfile$"                   "__"               dockerfile-mode)
            ("/docker-compose.yml$"           "__"               yaml-mode)
            ("/Makefile$"                     "__"               makefile-gmake-mode)
            ;; elisp
            ("\\.el$"                         "__initfile"       emacs-lisp-mode)
            ("/.dir-locals.el$"               nil)
            (snippet-mode "__" snippet-mode)
            ;; C/C++
            ("\\.h$"                           "__h"              c-mode)
            ("\\.c$"                           "__c"              c-mode)
            ("\\.h\\(h\\|pp|xx\\)$"            "__hpp"            c++-mode)
            ("\\.\\(cc\\|cpp\\)$"              "__cpp"            c++-mode)
            ("/main\\.\\(cc\\|cpp\\)$"         "__main.cpp"       c++-mode)
            ("/win32_\\.\\(cc\\|cpp\\)$"       "__winmain.cpp"    c++-mode)
            ;; go
            ("\\.go$"                          "__.go"            go-mode)
            ("/main\\.go$"                     "__main.go"        go-mode t)
            ;; web-mode
            ("\\.html$"                        "__.html"          web-mode)
            ("\\.scss$"                        "__"               scss-mode)
            ("/master\\.scss$"                 "__master.scss"    scss-mode)
            ("/normalize\\.scss$"              "__normalize.scss" scss-mode)
            ;; java
            ("/src/.+\\.java$"                 "__"               java-mode)
            ("/main\\.java$"                   "__main"           java-mode)
            ("/build\\.gradle$"                "__build.gradle"   android-mode)
            ;; javascript
            ("\\.\\(json\\|jshintrc\\)$"       "__"                  json-mode)
            ("/package\\.json$"                "__package.json"      json-mode)
            ("/bower\\.json$"                  "__bower.json"        json-mode)
            ("/gulpfile\\.js$"                 "__gulpfile.js"       js-mode)
            ("/webpack\\.config\\.js$"         "__webpack.config.js" js-mode)
            ;; Lua
            ("/main\\.lua$"                    "__main.lua"       love-mode)
            ("/conf\\.lua$"                    "__conf.lua"       love-mode)
            ;; Markdown
            ("\\.md$"                          "__"               markdown-mode)
            ;; Org
            ("\\.org$"                                          "__"            org-mode)
            ;; PHP
            ("\\.php$"                         "__"               php-mode)
            ("\\.class\\.php$"                 "__.class.php"     php-mode)
            ;; Python
            ;;("tests?/test_.+\\.py$"         "__"                 nose-mode)
            ;;("/setup\\.py$"                 "__setup.py"         python-mode)
            ("\\.py$"                          "__"               python-mode)
            ;; Ruby
            ("\\.rb$"                          "__"               ruby-mode)
            ("/Rakefile$"                      "__Rakefile"       ruby-mode t)
            ("/Gemfile$"                       "__Gemfile"        ruby-mode t)
            ("/\\.rspec$"                      "__.rspec"         rspec-mode)
            ("\\.gemspec$"                     "__.gemspec"       ruby-mode t)
            ("/spec_helper\\.rb$"              "__helper"         rspec-mode t)
            ("/lib/.+\\.rb$"                   "__module"         ruby-mode t)
            ("_spec\\.rb$"                     "__"               rspec-mode t)
            ;; Rust
            ("/main\\.rs$"                     "__main.rs"        rust-mode)
            ("/Cargo.toml$"                    "__Cargo.toml"     rust-mode)
            ;; Slim
            ("/\\(index\\|main\\)\\.slim$"     "__"               slim-mode)
            ;; Shell scripts
            ("/home/amos/git/serverconfig/scripts/.+"   "__"   sh-mode)
            ("/home/amos/git/serverconfig/.config/fish/functions/.+" "__func" fish-mode)
            ("\\.z?sh$"                        "__"               sh-mode)
            ("\\.fish$"                        "__"               fish-mode)
            ("\\.zunit$"                       "__zunit"          sh-mode)))))

(add-to-list 'auto-mode-alist '("/home/amos/git/serverconfig/scripts/.+" . sh-mode) 'append)

(defun +file-templates-get-short-path ()
  (when (string-match "/modules/\\(.+\\)$" buffer-file-truename)
    (match-string 1 buffer-file-truename)))

(defun +file-templates/insert-license ()
  "Insert a license file template into the current file."
  (interactive)
  (require 'yasnippet)
  (let* ((templates
          (let ((yas-choose-tables-first nil) ; avoid prompts
                (yas-choose-keys-first nil))
            (cl-loop for tpl in (yas--all-templates (yas--get-snippet-tables 'text-mode))
                     for uuid = (yas--template-uuid tpl)
                     if (string-prefix-p "__license-" uuid)
                     collect (cons (string-remove-prefix "__license-" uuid) tpl))))
         (uuid (yas-choose-value (mapcar #'car templates))))
    (when uuid
      (yas-expand-snippet (cdr (assoc uuid templates))))))

(defun +amos/projectile-find-other-file ()
  (interactive)
  (if (and (boundp 'cc-playground-mode) cc-playground-mode)
      (cc-switch-between-src-and-test)
    (require 'projectile)
    (projectile-find-other-file)))

(setq interprogram-paste-function #'x-get-selection)
(setq +popup-default-alist
      '((slot . 1)
        (vslot . -1)
        (side . right)
        (size . 0.5)
        (reusable-frames . visible)))
(map-put +popup-default-parameters 'modeline t)
(advice-add #'hide-mode-line-mode :override #'ignore)

;; ("^\\*Compil\\(ation\\|e-Log\\)" '((pop-up-frames . nil)) :select t :ttl 0 :quit t)

(defun first-non-dired-buffer () (loop for b in (buffer-list) if (not (with-current-buffer b (derived-mode-p 'dired-mode))) return b))

(set-popup-rules!
  '(("^\\*"  :slot 1 :vslot -1 :select t)
    ("^ \\*" :slot 1 :vslot -1 :size +popup-shrink-to-fit))
  '(("^\\*Completions"
     :slot -1 :vslot -2 :ttl 0)
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
     :slot 2 :vslot 2 :size 0.35 :select t)
    ;; `Info-mode'
    ("^\\*info\\*$"
     :slot 2 :vslot 2 :size 0.45 :select t)

    ("^\\(?:\\*magit\\|magit:\\)" :ignore t)

    ("^\\*ivy-occur"
     :side right :size 0.9 :select t))
  '(("^\\*Backtrace" :vslot 99 :size 0.4 :quit nil)))

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
  (apply orig-fun args)
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
      (message (kill-new (concat "b " filename ":" (number-to-string (line-number-at-pos)))))
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

(defun +amos/tmux-fork-window ()
  "Detach if inside tmux."
  (interactive)
  (+amos-store-jump-history)
  (shell-command! (format "tmux switch-client -t amos; tmux run -t amos \"tmux new-window -c %s\"" default-directory)))

;; (defun +amos/tmux-fork-window ()
;;   "Detach if inside tmux."
;;   (interactive)
;;   (+amos-store-jump-history)
;;   (shell-command! "tmux split -h"))

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

(defun +amos*lsp--xref-make-item (filename location)
  "Return a xref-item from a LOCATION in FILENAME."
  (let* ((range (gethash "range" location))
         (pos-start (gethash "start" range))
         (pos-end (gethash "end" range))
         (line (lsp--extract-line-from-buffer pos-start))
         (start (gethash "character" pos-start))
         (end (gethash "character" pos-end))
         (len (length line)))
    (add-face-text-property (max (min start len) 0)
                            (max (min end len) 0)
                            'ivy-minibuffer-match-face-2 t line)
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

(require 'company)
(require 'company-tng)
(require 'company-lsp)

(setq-default company-idle-delay 0.3
              company-auto-complete nil
              company-tooltip-limit 14
              company-dabbrev-downcase nil
              company-dabbrev-ignore-case nil
              company-dabbrev-code-other-buffers t
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

(defsubst +amos--is-frame-daemons-frame (f)
  (and (daemonp) (eq f terminal-frame)))

(defun +amos--frame-list-without-daemon ()
  "Return a list of frames without the daemon's frame."
  (if (daemonp)
      (filtered-frame-list
       #'(lambda (f) (not (+amos--is-frame-daemons-frame f))))
    (frame-list)))

(defun +amos/workspace-new ()
  (interactive)
  (let ((name (frame-parameter nil 'name))
        (oframe (selected-frame)))
    (make-frame-invisible oframe t)
    (select-frame (if (s-starts-with? "F" name)
                      (make-frame)
                    (make-frame `((name . ,name))))))
  (setq +amos--frame-list (reverse (+amos--frame-list-without-daemon))))

(setq +amos-tmux-need-switch nil)
(setq +amos-frame-stack nil)
(add-hook 'after-make-frame-functions (lambda (f) (if (frame-visible-p f) (push f +amos-frame-stack))))

(defun +amos/workspace-delete ()
  (interactive)
  (let ((f (selected-frame)))
    (setq +amos-frame-stack (-remove (lambda (e) (eq f e)) +amos-frame-stack))
    (if +amos-tmux-need-switch
        (if +amos-frame-stack
            (+amos/workspace-switch-to-frame (car +amos-frame-stack)))
      (+amos-workspace-cycle -1))
    (delete-frame f))
  (setq +amos--frame-list (reverse (+amos--frame-list-without-daemon)))
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
    (realign-windows)
    (recenter)))

(defun +amos/workspace-switch-to (index)
  (interactive)
  (when (< index (length +amos--frame-list))
    (let ((frame (nth index +amos--frame-list)))
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
  (let* ((n (length +amos--frame-list))
         (index (-elem-index (selected-frame) +amos--frame-list))
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
      (dired-omit-mode)
      (+amos-store-jump-history))))

(define-advice dired-revert (:after (&rest _) +amos*dired-revert)
  "Call `recenter' after `dired-revert'."
  (condition-case nil
      (+amos/recenter)
    (error nil)))

(after! wdired (evil-set-initial-state 'wdired-mode 'normal))
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
  (push 'dired-mode evil-snipe-disabled-modes))

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
 "M-t"            #'sp-transpose-sexp
 "C-t"            #'sp-transpose-hybrid-sexp
 "M-d"            #'sp-kill-sexp
 "C-o"            #'sp-kill-hybrid-sexp
 [M-backspace]    #'sp-backward-kill-sexp
 [134217855]      #'sp-backward-kill-sexp ; M-DEL
 "M-w"            #'sp-copy-sexp
 "M-("            #'wrap-with-parens
 "M-{"            #'wrap-with-braces
 "M-'"            #'wrap-with-single-quotes
 "M-\""           #'wrap-with-double-quotes
 "M-_"            #'wrap-with-underscores
 "M-`"            #'wrap-with-back-quotes)

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

(defun +amos*remove-git-index-lock (&rest _)
  (ignore-errors
    (delete-file ".git/index.lock")))
(advice-add #'magit-refresh :before #'+amos*remove-git-index-lock)

(defvar postpone-auto-revert-buffers nil)

(defvar postpone-auto-revert-interval nil)

(defadvice auto-revert-buffers (around maybe-postpone-auto-revert-buffers)
  "Delay `auto-revert-buffers' if `postpone-auto-revert-buffers' is non-nil."
  (if postpone-auto-revert-buffers
      ;; Do not run `auto-revert-buffers', but make its timer run more
      ;; frequently in the meantime, so that it will run promptly once
      ;; it's safe.  Remember the original `auto-revert-interval'.
      (unless postpone-auto-revert-interval
        (setq postpone-auto-revert-interval auto-revert-interval)
        (setq auto-revert-interval 0.5)
        (auto-revert-set-timer))
    ;; We are no longer postponed, so restore the original
    ;; `auto-revert-interval', and run `auto-revert-buffers'.
    (when postpone-auto-revert-interval
      (setq auto-revert-interval postpone-auto-revert-interval)
      (setq postpone-auto-revert-interval nil)
      (auto-revert-set-timer))
    ad-do-it)) ;; Run `auto-revert-buffers'.

(ad-activate 'auto-revert-buffers)

(defun +amos*magit-process-filter (&rest _)
  (setq postpone-auto-revert-buffers t))
(advice-add #'magit-process-filter :before #'+amos*magit-process-filter)

(defun +amos*magit-process-finish (&rest _)
  (setq postpone-auto-revert-buffers nil))
(advice-add #'magit-process-filter :before #'+amos*magit-process-filter)

(after! iedit
  (add-hook! 'iedit-mode-end-hook (+amos/recenter) (setq iedit-unmatched-lines-invisible nil)))

(after! subword
  (progn
    (define-category ?U "Uppercase")
    (define-category ?u "Lowercase")
    (modify-category-entry (cons ?A ?Z) ?U)
    (modify-category-entry (cons ?a ?z) ?u)
    (make-variable-buffer-local 'evil-cjk-word-separating-categories)
    (add-hook 'subword-mode-hook (lambda! (if subword-mode (push '(?u . ?U) evil-cjk-word-separating-categories)
                                       (setq evil-cjk-word-separating-categories (default-value 'evil-cjk-word-separating-categories)))))))

(after! magit
  (setq
   magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1
   magit-display-buffer-noselect t
   magit-revision-show-gravatars '("^Author:     " . "^Commit:     ")))

(after! evil-magit
  (setq evil-magit-use-z-for-folds nil))

(after! org (setq org-image-actual-width '(400)))

(after! recentf
  (setq recentf-max-saved-items 10000))

(after! flycheck
  (setq flycheck-check-syntax-automatically '(save mode-enabled)))

(advice-add #'evil-multiedit--cycle :after #'+amos/recenter)
(advice-add #'evil-multiedit-match-and-next :after #'+amos/recenter)

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
  (advice-add 'company-call-backend :before-until 'company-tng--supress-post-completion)
  (company-complete-selection)
  (call-interactively (key-binding (this-command-keys))))

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
        (setf (evil-jumps-struct-idx jump-struct) idx)))

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

;; (add-hook! 'minibuffer-setup-hook (setq truncate-lines t)) ;; TODO breaks ivy minibuffer

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

(defun rgc-lua-calculate-indentation-override (old-function &rest arguments)
  (or (lua-busted-fuckups-fix)
      (apply old-function arguments)))

(advice-add #'lua-calculate-indentation-override
            :around #'rgc-lua-calculate-indentation-override)

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

(evil-define-operator evil-ex-substitute
  (beg end pattern replacement flags)
  "The Ex substitute command.
\[BEG,END]substitute/PATTERN/REPLACEMENT/FLAGS"
  :repeat nil
  :jump t
  :move-point nil
  :motion evil-line
  (interactive "<r><s/>")
  (evil-ex-nohighlight)
  (unless pattern
    (user-error "No pattern given"))
  (setq replacement (or replacement ""))
  (setq evil-ex-last-was-search nil)
  (let* ((flags (append flags nil))
         (count-only (memq ?n flags))
         (confirm (and (memq ?c flags) (not count-only)))
         (case-fold-search (evil-ex-pattern-ignore-case pattern))
         (case-replace case-fold-search)
         (evil-ex-substitute-regex (evil-ex-pattern-regex pattern))
         (evil-ex-substitute-nreplaced 0)
         (evil-ex-substitute-last-point (point))
         (whole-line (evil-ex-pattern-whole-line pattern))
         (evil-ex-substitute-overlay (make-overlay (point) (point)))
         (evil-ex-substitute-hl (evil-ex-make-hl 'evil-ex-substitute))
         (orig-point-marker (move-marker (make-marker) (point)))
         (end-marker (move-marker (make-marker) end))
         (use-reveal confirm)
         reveal-open-spots
         zero-length-match
         match-contains-newline
         transient-mark-mode)
    (setq evil-ex-substitute-pattern pattern
          evil-ex-substitute-replacement replacement
          evil-ex-substitute-flags flags
          isearch-string evil-ex-substitute-regex)
    (isearch-update-ring evil-ex-substitute-regex t)
    (unwind-protect
        (progn
          (evil-ex-hl-change 'evil-ex-substitute pattern)
          (overlay-put evil-ex-substitute-overlay 'face 'isearch)
          (overlay-put evil-ex-substitute-overlay 'priority 1001)
          (goto-char beg)
          (catch 'exit-search
            (while (re-search-forward evil-ex-substitute-regex end-marker t)
              (when (not (and query-replace-skip-read-only
                              (text-property-any (match-beginning 0) (match-end 0) 'read-only t)))
                (let ((match-str (match-string 0))
                      (match-beg (move-marker (make-marker) (match-beginning 0)))
                      (match-end (move-marker (make-marker) (match-end 0)))
                      (match-data (match-data)))
                  (goto-char match-beg)
                  (setq match-contains-newline
                        (string-match-p "\n" (buffer-substring-no-properties
                                              match-beg match-end)))
                  (setq zero-length-match (= match-beg match-end))
                  (when (and (string= "^" evil-ex-substitute-regex)
                             (= (point) end-marker))
                    ;; The range (beg end) includes the final newline which means
                    ;; end-marker is on one line down. With the regex "^" the
                    ;; beginning of this last line will be matched which we don't
                    ;; want, so we abort here.
                    (throw 'exit-search t))
                  (setq evil-ex-substitute-last-point match-beg)
                  (if confirm
                      (let ((prompt
                             (format "Replace %s with %s (y/n/a/q/l/e/^E/^Y)? "
                                     match-str
                                     (evil-match-substitute-replacement
                                      evil-ex-substitute-replacement
                                      (not case-replace))))
                            (search-invisible t)
                            response)
                        (move-overlay evil-ex-substitute-overlay match-beg match-end)
                        ;; Simulate `reveal-mode'. `reveal-mode' uses
                        ;; `post-command-hook' but that won't work here.
                        (when use-reveal
                          (reveal-post-command))
                        (catch 'exit-read-char
                          (while (setq response (read-char prompt))
                            (if (= response ?e)
                                (setf (cdr evil-ex-substitute-replacement)
                                      (read-string "replace-string: " (cdr evil-ex-substitute-replacement))))
                            (when (member response '(?y ?a ?l ?e))
                              (unless count-only
                                (set-match-data match-data)
                                (evil-replace-match evil-ex-substitute-replacement
                                                    (not case-replace)))
                              (setq evil-ex-substitute-nreplaced
                                    (1+ evil-ex-substitute-nreplaced))
                              (evil-ex-hl-set-region 'evil-ex-substitute
                                                     (save-excursion
                                                       (forward-line)
                                                       (point))
                                                     (evil-ex-hl-get-max
                                                      'evil-ex-substitute)))
                            (cl-case response
                              ((?y ?n ?e) (throw 'exit-read-char t))
                              (?a (setq confirm nil)
                                  (throw 'exit-read-char t))
                              ((?q ?l ?\C-\[) (throw 'exit-search t))
                              (?\C-e (evil-scroll-line-down 1))
                              (?\C-y (evil-scroll-line-up 1))))))
                    (setq evil-ex-substitute-nreplaced
                          (1+ evil-ex-substitute-nreplaced))
                    (unless count-only
                      (set-match-data match-data)
                      (evil-replace-match evil-ex-substitute-replacement
                                          (not case-replace))))
                  (goto-char match-end)
                  (cond ((>= (point) end-marker)
                         ;; Don't want to perform multiple replacements at the end
                         ;; of the search region.
                         (throw 'exit-search t))
                        ((and (not whole-line)
                              (not match-contains-newline))
                         (forward-line)
                         ;; forward-line just moves to the end of the line on the
                         ;; last line of the buffer.
                         (when (or (eobp)
                                   (> (point) end-marker))
                           (throw 'exit-search t)))
                        ;; For zero-length matches check to see if point won't
                        ;; move next time. This is a problem when matching the
                        ;; regexp "$" because we can enter an infinite loop,
                        ;; repeatedly matching the same character
                        ((and zero-length-match
                              (let ((pnt (point)))
                                (save-excursion
                                  (and
                                   (re-search-forward
                                    evil-ex-substitute-regex end-marker t)
                                   (= pnt (point))))))
                         (if (or (eobp)
                                 (>= (point) end-marker))
                             (throw 'exit-search t)
                           (forward-char)))))))))
      (evil-ex-delete-hl 'evil-ex-substitute)
      (delete-overlay evil-ex-substitute-overlay)

      (if count-only
          (goto-char orig-point-marker)
        (goto-char evil-ex-substitute-last-point))

      (move-marker orig-point-marker nil)
      (move-marker end-marker nil)

      (when use-reveal
        (evil-revert-reveal reveal-open-spots)))

    (message "%s %d occurrence%s"
             (if count-only "Found" "Replaced")
             evil-ex-substitute-nreplaced
             (if (/= evil-ex-substitute-nreplaced 1) "s" ""))
    (evil-first-non-blank)))

;; (add-hook! 'doom-load-theme-hook
;;   (set-face-attribute 'mode-line-inactive nil
;;                       :underline t
;;                       :background (face-background 'default)))

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
        (add-hook 'post-command-hook 'lsp-ui-imenu--post-command nil t)
        ))
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
  ;; wipe
  (+amos/close-current-buffer t))

(defun +amos/kill-current-buffer ()
  (interactive)
  ;; wipe and kill
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

(defun +amos/projectile-find-file-no-cache ()
  (interactive)
  (projectile-invalidate-cache nil)
  (projectile-find-file))

(defun +amos/counsel-recentf-no-cache ()
  (interactive)
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
  (text-scale-decrease 0.0))

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

(defun find-file--line-number (orig-fun filename &optional wildcards)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename)))
      (apply orig-fun (list filename wildcards))
      (when line-number
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-number))
        (+amos/recenter)))))

(advice-add 'find-file :around #'find-file--line-number)

(add-hook! 'minibuffer-setup-hook
  (setq-local truncate-lines t)
  (setq-local inhibit-message t))

(mapc #'evil-declare-change-repeat
      '(company-complete-mouse
        ;; +amos/maybe-add-end-of-statement
        +amos/company-abort
        amos-company-files
        company-complete-selection
        company-complete-common))

(mapc #'evil-declare-ignore-repeat
      '(
        +amos/align-repeat-left
        +amos/align-repeat-right
        +amos/all-substitute
        +amos/avy-goto-char-timer
        +amos/counsel-projectile-switch-project
        +amos/counsel-recentf-no-cache
        +amos/counsel-rg-cur-dir
        +amos/decrease-zoom
        +amos/dired-jump
        +amos/direnv-reload
        +amos/dump-evil-jump-list
        +amos/increase-zoom
        +amos/kill-current-buffer
        +amos/line-substitute
        +amos/lsp-ui-imenu
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
