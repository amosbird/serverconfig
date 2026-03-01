;;; post-init.el --- Amos's Emacs configuration -*- lexical-binding: t; -*-
;;
;; Lean Emacs configuration based on minimal-emacs.d.
;; Architecture: early-init.el / init.el come from minimal-emacs.d (do not modify);
;;               all customizations go in pre-early-init.el, pre-init.el, and this file.
;;
;; Table of contents:
;;   §1  UI & Theme           - Appearance, modeline, line numbers, highlights
;;   §2  Evil Modal Editing   - Full Vim emulation + general.el leader key
;;   §3  Editing Enhancements - Parens, indentation, undo, navigation
;;   §4  Completion Framework - Vertico + Orderless + Corfu + Consult + Embark
;;   §5  Version Control      - Magit, diff-hl, git-link
;;   §6  LSP & Diagnostics    - Eglot (built-in), Flymake
;;   §7  Language Support      - treesit-auto + language modes
;;   §8  Org mode             - Notes, GTD, LaTeX, blogging
;;   §9  Tools                - direnv, yasnippet, formatting, etc.
;;   §10 Key Bindings         - Meow leader key system (SPC)
;;   §11 Server               - emacsclient support

;;; Code:

;; ============================================================================
;; §1 UI & Theme
;; ============================================================================

;;;; Theme — doom-themes provides rich themes; using doom-one here
;; Options: doom-one, doom-vibrant, doom-dracula, doom-monokai-pro, etc.
;; Full list: M-x consult-theme or https://github.com/doomemacs/themes
(use-package doom-themes
  :demand t
  :custom
  (doom-themes-enable-bold nil)         ; Disable bold (consistent with original Doom config)
  (doom-themes-enable-italic t)
  :config
  (load-theme 'doom-one t)
  (doom-themes-org-config))

;;;; Modeline — doom-modeline provides an elegant status bar
;; Disable icons (not supported in terminal), minimize height to save space
(use-package doom-modeline
  :demand t
  :custom
  (doom-modeline-icon nil)              ; No icons in terminal
  (doom-modeline-height 1)              ; Minimal height
  (doom-modeline-bar-width 0)           ; No left-side bar
  (doom-modeline-buffer-file-name-style 'truncate-with-project)
  (doom-modeline-minor-modes nil)       ; Hide minor modes
  :config
  ;; Enable standard Emacs modes that doom-modeline's buffer-position segment depends on
  (line-number-mode 1)
  (column-number-mode 1)
  (size-indication-mode 1)

  ;; Treat modeline as always active (no dim on unfocused window)
  (advice-add #'doom-modeline--active :override (lambda () t))

  ;; Workspace tab indicator — show numbered frame tabs in modeline
  (defface amos/workspace-tab-selected
    '((t (:inherit highlight)))
    "Face for the active workspace tab in the modeline.")
  (defface amos/workspace-tab
    '((t (:foreground "#bbc2cf" :inherit mode-line)))
    "Face for inactive workspace tabs in the modeline.")

  (defvar amos/full-width-digits
    ["１" "２" "３" "４" "５" "６" "７" "８" "９"]
    "Full-width digit characters for workspace tab display.")

  (defun amos/frame-modeline-string (&optional _)
    "Build workspace tab string for the modeline: |１|２|３|."
    (let ((frames (amos/tty-root-frame-list))
          (current (amos/selected-root-frame)))
      (concat " |"
              (mapconcat
               #'identity
               (cl-loop for frame in frames
                        for i from 0
                        collect
                        (propertize
                         (if (< i 9)
                             (aref amos/full-width-digits i)
                           (format "%d" (1+ i)))
                         'face (if (eq current frame)
                                   'amos/workspace-tab-selected
                                 'amos/workspace-tab)))
               "|")
              "|")))

  (doom-modeline-def-segment frame (amos/frame-modeline-string))

  ;; Hostname segment
  (defvar amos/modeline-hostname
    (propertize (concat "  " (or (getenv "HOSTNAME") (system-name)) " ")
                'face '(:weight bold :foreground "#51afef"))
    "Cached hostname string for modeline.")
  (doom-modeline-def-segment host amos/modeline-hostname)

  ;; Custom modeline layout with frame tabs and hostname
  (doom-modeline-def-modeline 'amos
    '(bar matches follow buffer-info buffer-position word-count parrot selection-info frame)
    '(lsp indent-info buffer-encoding major-mode process vcs check time))

  ;; Custom format function: fills gap between LHS and RHS with dashes
  (defun amos/string-pixel-width (str)
    "Return pixel width of STR in mode-line face."
    (* (string-width str) (window-font-width nil 'mode-line)))

  (defun doom-modeline-format--amos ()
    "Custom modeline format with right-aligned RHS."
    (let ((lhs-forms (doom-modeline--prepare-segments
                      '(bar matches follow buffer-info buffer-position word-count parrot selection-info frame)))
          (rhs-forms (doom-modeline--prepare-segments
                      '(lsp indent-info buffer-encoding major-mode process vcs check time))))
      (list lhs-forms
            (let* ((lhs-str (format-mode-line (cons "" lhs-forms) nil nil (current-buffer)))
                   (lhs-len (length lhs-str))
                   (lhs-width (progn
                                (add-face-text-property 0 lhs-len 'mode-line t lhs-str)
                                (amos/string-pixel-width lhs-str)))
                   (rhs-str (format-mode-line (cons "" rhs-forms) nil nil (current-buffer)))
                   (rhs-len (length rhs-str))
                   (rhs-width (progn
                                (add-face-text-property 0 rhs-len 'mode-line t rhs-str)
                                (amos/string-pixel-width rhs-str))))
              (propertize (make-string (max 0 (- (frame-pixel-width) lhs-width rhs-width)) ?-)
                          'face `(:foreground ,(face-background 'mode-line))))
            rhs-forms)))

  ;; Override doom-modeline-set-modeline to always use our layout
  (advice-add #'doom-modeline-set-modeline :override
              (lambda (&rest _)
                (setq mode-line-format (list "%e" '(:eval (doom-modeline-format--amos))))
                (setq-default mode-line-format (list "%e" '(:eval (doom-modeline-format--amos))))))

  (doom-modeline-mode 1)

  ;; Fix buffers created before doom-modeline (e.g. *Messages*) that have
  ;; buffer-local mode-line-format set to nil by early-init.el
  (let ((ml (default-value 'mode-line-format)))
    (dolist (buf (buffer-list))
      (with-current-buffer buf
        (when (local-variable-p 'mode-line-format)
          (setq mode-line-format ml)))))

  ;; Prevent window-font-height from calling select-window (triggers evil-set-cursor in child frames)
  (advice-add #'window-font-height :override (lambda (&rest _) 1))
  ;; Don't update modeline on focus change (causes needless cursor refreshes)
  (setq after-focus-change-function #'ignore))

;;;; Line numbers — display relative line numbers in code/text buffers
(setq-default display-line-numbers-type 'relative)
(setq-default display-line-numbers-width-start t)  ; Pre-allocate width to prevent column jitter
(add-hook 'prog-mode-hook #'display-line-numbers-mode)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'conf-mode-hook #'display-line-numbers-mode)
;; *scratch* is created before init; hooks won't fire, so enable manually
(add-hook 'emacs-startup-hook
          (lambda ()
            (when-let* ((buf (get-buffer "*scratch*")))
              (with-current-buffer buf
                (display-line-numbers-mode 1)))))

;;;; Highlight current line — easier cursor tracking
(add-hook 'prog-mode-hook #'hl-line-mode)
(add-hook 'text-mode-hook #'hl-line-mode)

;;;; hl-todo — highlight TODO/FIXME/HACK keywords in code
(use-package hl-todo
  :hook ((prog-mode . hl-todo-mode)
         (conf-mode . hl-todo-mode)))

;;;; Pulse on jump — briefly highlight current line on scroll/window switch
(defun amos/pulse-line (&rest _)
  "Briefly highlight the current line for visual feedback."
  (pulse-momentary-highlight-one-line (point)))
(dolist (cmd '(recenter-top-bottom scroll-up-command
                                   scroll-down-command other-window))
  (advice-add cmd :after #'amos/pulse-line))

;;;; which-key — show key binding hints after a prefix key
(use-package which-key
  :hook (after-init . which-key-mode)
  :custom
  (which-key-idle-delay 0.5)
  (which-key-sort-order 'which-key-key-order-alpha))

;;;; Miscellaneous UI settings
(setq-default line-spacing 0.1)         ; Line spacing
(setq visible-bell nil)                 ; Disable visible bell
(setq ring-bell-function #'ignore)      ; Disable audible bell

;; Window dividers — thin line separators; clean up vertical-border face in terminal
(setq window-divider-default-bottom-width 1
      window-divider-default-right-width 1
      window-divider-default-places t)
(add-hook 'after-init-hook #'window-divider-mode)
;; Use thin vertical line for window border in terminal
(unless (display-graphic-p)
  (set-display-table-slot standard-display-table 'vertical-border ?│))
;; Clean up vertical-border background after theme load
(defun amos/clean-window-border ()
  (set-face-background 'vertical-border nil)
  (set-face-foreground 'vertical-border "gray30")
  (set-face-attribute 'mode-line-inactive nil
                      :inherit 'mode-line
                      :background 'unspecified
                      :foreground 'unspecified))
(add-hook 'after-init-hook #'amos/clean-window-border)
(advice-add 'load-theme :after (lambda (&rest _) (amos/clean-window-border)))

;; winner-mode — undo/redo window layout with C-c <left> / C-c <right>
(add-hook 'after-init-hook #'winner-mode)

;; display-buffer-alist — window placement rules (replaces Doom popup system)
(setq display-buffer-alist
      '(;; Compilation, grep — right side
        ("\\*\\(?:compilation\\|Compile-Log\\|grep\\|ripgrep\\)\\*"
         (display-buffer-in-side-window)
         (side . right) (window-width . 0.5) (slot . 0))
        ;; Help, Apropos — right side
        ("\\*\\(?:[Hh]elp\\|Apropos\\)\\*"
         (display-buffer-in-side-window)
         (side . right) (window-width . 0.5) (slot . 1))
        ;; Info — right side
        ("\\*info\\*"
         (display-buffer-in-side-window)
         (side . right) (window-width . 0.5) (slot . 2))
        ;; Man/WoMan — right side
        ("\\*\\(?:Wo\\)?Man "
         (display-buffer-in-side-window)
         (side . right) (window-width . 0.5) (slot . 0))
        ;; git-gutter diff popup — right side, don't select
        ("\\*git-gutter"
         (display-buffer-in-side-window)
         (side . right) (window-width . 0.5) (slot . 0)
         (inhibit-switch-frame . t))
        ;; diff-hl — right side, don't select
        ("\\*diff-hl"
         (display-buffer-in-side-window)
         (side . right) (window-width . 0.5) (slot . 0)
         (inhibit-switch-frame . t))
        ;; vc-diff — right side, don't select
        ("\\*vc-diff"
         (display-buffer-in-side-window)
         (side . right) (window-width . 0.5) (slot . 0)
         (inhibit-switch-frame . t))
        ;; vc-change — right side
        ("\\*vc-change"
         (display-buffer-in-side-window)
         (side . right) (window-width . 0.5) (slot . 0))
        ;; Customize — right side
        ("\\*Customize"
         (display-buffer-in-side-window)
         (side . right) (window-width . 0.5) (slot . 2))
        ;; HTTP Response/HTTP — right side, don't select
        ("\\*HTTP"
         (display-buffer-in-side-window)
         (side . right) (window-width . 0.5) (inhibit-switch-frame . t))
        ;; rmsbolt — right side, don't select
        ("\\*rmsbolt-output"
         (display-buffer-in-side-window)
         (side . right) (window-width . 0.5) (inhibit-switch-frame . t))
        ;; temp buffer — right side
        ("\\*temp\\*"
         (display-buffer-in-side-window)
         (side . right) (window-width . 0.5) (slot . 0))
        ;; Flycheck — bottom
        ("\\*Flycheck"
         (display-buffer-in-side-window)
         (side . bottom) (window-height . 0.5) (slot . 0))
        ;; Warnings, backtrace — bottom
        ("\\*\\(?:Warnings\\|Backtrace\\)\\*"
         (display-buffer-in-side-window)
         (side . bottom) (window-height . 0.25))
        ;; Calc — bottom
        ("\\*Calc"
         (display-buffer-in-side-window)
         (side . bottom) (window-height . 0.4) (slot . 0))
        ;; evil-registers — bottom
        ("\\*evil-registers"
         (display-buffer-in-side-window)
         (side . bottom) (window-height . 0.3) (slot . 0))
        ;; undo-tree — left side
        ("^ \\*undo-tree\\*"
         (display-buffer-in-side-window)
         (side . left) (window-width . 20) (slot . 0))
        ;; aider — left side, don't select
        ("\\*aider:"
         (display-buffer-in-side-window)
         (side . left) (window-width . 0.25) (inhibit-switch-frame . t))
        ;; Embark export — bottom
        ("\\*Embark.*Export"
         (display-buffer-in-side-window)
         (side . bottom) (window-height . 0.35))
        ;; Magit, transient — same window (fullframe-like)
        ("\\(?:magit:\\| \\*transient\\*\\)"
         (display-buffer-same-window))
        ;; Embark writable export — same window
        ("\\*Embark Writable Export"
         (display-buffer-same-window))))

;; CJK font — set fallback font for Chinese/Japanese/Korean characters
(defun amos/init-cjk-font (&optional frame)
  (when (display-graphic-p (or frame (selected-frame)))
    (dolist (charset '(kana han cjk-misc bopomofo))
      (set-fontset-font t charset
                        (font-spec :family "WenQuanYi Micro Hei" :size 34)))))
(if (display-graphic-p)
    (amos/init-cjk-font)
  (add-hook 'after-make-frame-functions #'amos/init-cjk-font))

;; Frame title — show buffer name and project
(setq frame-title-format '("%b — Emacs"))

;; Frame-based workspace system — each "workspace" is a TTY root frame
(defun amos/selected-root-frame ()
  "Return the root frame (skip child frames)."
  (let ((frame (selected-frame)))
    (or (frame-parent frame) frame)))

(defun amos/tty-root-frame-list ()
  "Return a list of TTY root frames (non-daemon, non-child)."
  (let (result)
    (dolist (f (frame-list))
      (when (and (frame-terminal f)
                 (frame-parameter f 'tty-type)
                 (not (frame-parent f)))
        (push f result)))
    result))

(defun amos/workspace-new ()
  "Create a new workspace (TTY frame), hiding the current one."
  (interactive)
  (let ((oframe (amos/selected-root-frame))
        (dir default-directory))
    (make-frame-invisible oframe t)
    (let ((nframe (make-frame)))
      (select-frame nframe)
      (switch-to-buffer "*scratch*")
      (cd dir)
      (force-mode-line-update t))))

(defun amos/workspace-delete ()
  "Delete current workspace frame, switch to most recently used."
  (interactive)
  (let (best-frame best-time)
    (dolist (window (window-list-1 nil 'nomini t))
      (let ((time (window-use-time window))
            (frame (or (frame-parent (window-frame window))
                       (window-frame window))))
        (when (and (not (eq frame (amos/selected-root-frame)))
                   (or (not best-time) (> time best-time)))
          (setq best-time time)
          (setq best-frame frame))))
    (delete-frame)
    (when (and best-frame (frame-live-p best-frame))
      (select-frame best-frame)
      (force-mode-line-update t))))

(defun amos/workspace-switch-to (index)
  "Switch to workspace frame at INDEX."
  (interactive)
  (let ((frames (amos/tty-root-frame-list)))
    (when (< index (length frames))
      (let ((frame (nth index frames)))
        (select-frame frame)
        (raise-frame frame)
        (recenter)
        (force-mode-line-update t)))))

(defun amos/workspace-cycle (off)
  "Cycle through workspaces by offset OFF."
  (let* ((frames (amos/tty-root-frame-list))
         (n (length frames))
         (index (cl-position (amos/selected-root-frame) frames))
         (i (% (+ off index n) n)))
    (amos/workspace-switch-to i)))

(defun amos/workspace-switch-left ()  (interactive) (amos/workspace-cycle -1))
(defun amos/workspace-switch-right () (interactive) (amos/workspace-cycle +1))

;; M-1..M-9 switch to workspace, C-9/C-0 cycle, C-x c new, C-x k delete
(dolist (i '(1 2 3 4 5 6 7 8 9))
  (global-set-key (kbd (format "M-%d" i))
                  (let ((idx (1- i)))
                    (lambda () (interactive) (amos/workspace-switch-to idx)))))
(global-set-key (kbd "C-9") #'amos/workspace-switch-left)
(global-set-key (kbd "C-0") #'amos/workspace-switch-right)
(global-set-key (kbd "C-x c") #'amos/workspace-new)
(global-set-key (kbd "C-x k") #'amos/workspace-delete)

;; Terminal key translations — kitty remaps ctrl+9/0/,/. to S-F9/F10/F1/F2
;; and ctrl+shift+s/d/f to S-F4/F5/F11
(global-set-key (kbd "S-<f9>")  #'amos/workspace-switch-left)
(global-set-key (kbd "S-<f10>") #'amos/workspace-switch-right)
(global-set-key (kbd "S-<f1>")  #'previous-buffer)
(global-set-key (kbd "S-<f2>")  #'next-buffer)
(global-set-key (kbd "C-,")     #'previous-buffer)
(global-set-key (kbd "C-.")     #'next-buffer)
;; C-S-s → ripgrep project, C-S-d → ripgrep current dir (GUI: C-S-s/C-S-d, terminal: S-F4/S-F5)
(global-set-key (kbd "C-S-s")   #'amos/consult-ripgrep)
(global-set-key (kbd "C-S-d")   #'amos/consult-ripgrep-cur-dir)
(global-set-key (kbd "S-<f4>")  #'amos/consult-ripgrep)
(global-set-key (kbd "S-<f5>")  #'amos/consult-ripgrep-cur-dir)

;; ============================================================================
;; §2 Evil Modal Editing
;; ============================================================================
;;
;; Evil provides full Vim emulation including all keybindings, text objects, and macros.
;; evil-collection provides Evil keybindings for built-in and third-party modes.
;; general.el defines the SPC leader key system (similar to Doom Emacs).

(use-package evil
  :demand t
  :custom
  (evil-want-integration t)             ; Required by evil-collection
  (evil-want-keybinding nil)            ; Let evil-collection manage keybindings
  (evil-want-C-u-scroll t)             ; C-u scrolls up (Vim convention)
  (evil-want-C-d-scroll t)             ; C-d scrolls down
  (evil-want-Y-yank-to-eol t)          ; Y yanks to end of line (like D/C)
  (evil-undo-system 'undo-redo)         ; Use Emacs 28+ built-in undo-redo
  (evil-search-module 'evil-search)     ; Use evil search (supports / ? n N)
  (evil-symbol-word-search t)            ; # and * search by symbol, not word
  (evil-ex-search-vim-style-regexp t)    ; \( \) \| in ex search (Vim compat)
  (evil-ex-interactive-search-highlight 'selected-window) ; Only highlight in current window
  (evil-split-window-below t)           ; :sp opens new window below
  (evil-vsplit-window-right t)          ; :vs opens new window to the right
  (evil-want-fine-undo t)              ; Fine-grained undo in insert mode
  (evil-esc-delay 0)                    ; ESC is immediate, never part of terminal escape sequences
  :config
  (evil-mode 1)
  ;; Must call evil-select-search-module (not just setq) to rebind * # n N
  ;; to the evil-ex-search variants that support persistent highlights.
  (evil-select-search-module 'evil-search-module 'evil-search)
  ;; Fix uninitialized evil-mode-buffers (Evil 1.15.0)
  (unless (boundp 'evil-mode-buffers)
    (setq evil-mode-buffers nil))

  ;; Unbind SPC in special-mode-map so the general.el override leader works
  ;; in *Messages*, *Help*, etc. (special-mode-map binds SPC to scroll-up)
  (define-key special-mode-map " " nil)

  ;; Disable evil in child-frame buffers and temp buffers (prevents cursor flicker)
  (add-to-list 'evil-buffer-regexps '(" \\*corfu\\*" . nil))
  (add-to-list 'evil-buffer-regexps '(" \\*which-key\\*" . nil))
  (add-to-list 'evil-buffer-regexps '(" \\*eldoc\\*" . nil))
  (add-to-list 'evil-buffer-regexps '(" \\*temp\\*" . nil))
  (add-to-list 'evil-buffer-regexps '("\\*EGLOT" . nil))
  (add-to-list 'evil-buffer-regexps '("\\*jsonrpc" . nil))
  (add-to-list 'evil-buffer-regexps '(" \\*string-output\\*" . nil))
  (add-to-list 'evil-buffer-regexps '(" \\*string-pixel-width\\*" . nil))
  (add-to-list 'evil-buffer-regexps '("\\*Native-compile-Log\\*" . nil))

  ;; Cursor style — different colors/shapes per state for mode identification
  (setq evil-default-cursor #'ignore)    ; Don't reset cursor on every refresh (prevents flicker)
  (setq evil-normal-state-cursor  '("#b8860b" box))
  (setq evil-insert-state-cursor  '("#66cd00" bar))
  (setq evil-visual-state-cursor  '("#808080" hbar))
  (setq evil-replace-state-cursor '("#cd6600" hbar))
  (setq evil-emacs-state-cursor   '("#7ec0ee" box))
  (setq evil-motion-state-cursor  '("#cd96cd" box))

  ;; Skip cursor refresh in child frames (avoid glitches).
  ;; Only wrap evil-refresh-cursor, NOT evil-set-cursor — etcc's defadvice
  ;; on evil-set-cursor must fire to send terminal escape sequences.
  ;; This matches the Doom config approach.
  (defun amos/evil-refresh-cursor-a (orig-fn &rest args)
    "Skip evil cursor refresh when in a child frame."
    (unless (frame-parent (selected-frame))
      (apply orig-fn args)))
  (advice-add #'evil-refresh-cursor :around #'amos/evil-refresh-cursor-a)

  ;; Disable evil's select-window advice — it calls evil-refresh-cursor on
  ;; every window selection, including internal walk-windows during redisplay.
  (ad-deactivate 'select-window)

  ;; In normal/visual/motion state, prevent cursor from resting on the phantom
  ;; empty line after final newline, so display-line-numbers always shows the absolute number
  (defun amos/clamp-eob-cursor ()
    "Prevent cursor from resting on phantom empty line at end of buffer."
    (when (and (not (evil-insert-state-p))
               (not (evil-emacs-state-p))
               (eobp)
               (not (bobp))
               (eq (char-before) ?\n))
      (forward-line -1)))
  (add-hook 'post-command-hook #'amos/clamp-eob-cursor)
  (defvar amos/private-kill-ring nil)
  (defmacro amos/with-private-kill-ring (&rest body)
    "Execute BODY with a private kill-ring, leaving the system clipboard untouched."
    `(let (interprogram-cut-function
           (kill-ring amos/private-kill-ring))
       ,@body
       (setq amos/private-kill-ring kill-ring)))

  ;; Helper: enter insert state if not already in it before performing an action
  (defun amos/ensure-insert-state ()
    "Ensure Evil is in insert state."
    (unless (or (evil-insert-state-p) (active-minibuffer-window))
      (setq evil-insert-count 1 evil-insert-vcount nil)
      (evil-insert-state)))

  ;; M-d forward-kill-word — enters insert state if not already
  (evil-define-command amos/delete-forward-word ()
    (evil-signal-at-bob-or-eob 1)
    (amos/ensure-insert-state)
    (amos/with-private-kill-ring
     (kill-region (point)
                  (max (save-excursion
                         (if (looking-at "[ \t\r\n\v\f]")
                             (progn (re-search-forward "[^ \t\r\n\v\f]")
                                    (backward-char))
                           (forward-word 1))
                         (point))
                       (line-beginning-position)))))

  ;; M-DEL backward-kill-word — enters insert state if not already
  (evil-define-command amos/delete-backward-word ()
    (evil-signal-at-bob-or-eob -1)
    (unless (or (eolp) (evil-insert-state-p) (active-minibuffer-window))
      (amos/ensure-insert-state)
      (forward-char))
    (amos/with-private-kill-ring
     (kill-region (point)
                  (min (save-excursion
                         (if (looking-back "[ \t\r\n\v\f]" nil)
                             (progn (re-search-backward "[^ \t\r\n\v\f]")
                                    (forward-char))
                           (forward-word -1))
                         (point))
                       (line-end-position)))))

  ;; Delete single char (bypass kill-ring)
  (evil-define-command amos/delete-char ()
    (amos/with-private-kill-ring (delete-char 1 1)))
  (evil-define-command amos/delete-backward-char ()
    (amos/with-private-kill-ring (with-no-warnings (delete-backward-char 1 1))))

  ;; C-k kill to end of line
  (evil-define-command amos/kill-line ()
    (amos/with-private-kill-ring
     (kill-region (point) (line-end-position))))

  ;; C-<backspace>/C-w backward-kill to BOL or indentation
  (evil-define-command amos/backward-kill-to-bol ()
    (if (bolp) (amos/delete-backward-char)
      (let ((x (save-excursion (back-to-indentation) (point)))
            (y (point)))
        (when (>= x y) (setq x (line-beginning-position)))
        (amos/with-private-kill-ring (kill-region x y)))))

  ;; --- Subword movement (camelCase-aware) ---
  (defvar amos/subword-forward-regexp
    "\\W*\\(\\([[:upper:]]*\\(\\W\\)?\\)[[:lower:][:digit:]]*\\)"
    "Regexp used by subword-forward-internal.")
  (defvar amos/subword-backward-regexp
    "\\(\\(\\W\\|[[:lower:][:digit:]]\\)\\([[:upper:]]+\\W*\\)\\|\\W\\w+\\)"
    "Regexp used by subword-backward-internal.")

  (defun amos/subword-forward-internal ()
    (if (and (save-excursion
               (let ((case-fold-search nil))
                 (modify-syntax-entry ?_ "_")
                 (re-search-forward amos/subword-forward-regexp nil t)))
             (> (match-end 0) (point)))
        (goto-char
         (cond
          ((and (< 1 (- (match-end 2) (match-beginning 2)))
                (not (and (null (match-beginning 3))
                          (eq (match-end 2) (match-end 1)))))
           (1- (match-end 2)))
          (t (match-end 0))))
      (forward-word 1)))

  (defun amos/subword-backward-internal ()
    (if (save-excursion
          (let ((case-fold-search nil))
            (modify-syntax-entry ?_ "_")
            (re-search-backward amos/subword-backward-regexp nil t)))
        (goto-char
         (cond
          ((and (match-end 3)
                (< 1 (- (match-end 3) (match-beginning 3)))
                (not (eq (point) (match-end 3))))
           (1- (match-end 3)))
          (t (1+ (match-beginning 0)))))
      (backward-word 1)))

  (defconst amos/subword-find-word-boundary-function-table
    (let ((tab (make-char-table nil)))
      (set-char-table-range tab t #'amos/subword-find-word-boundary)
      tab))
  (defconst amos/subword-empty-char-table (make-char-table nil))

  (defun amos/subword-find-word-boundary (pos limit)
    (let ((find-word-boundary-function-table amos/subword-empty-char-table))
      (save-match-data
        (save-excursion
          (save-restriction
            (if (< pos limit)
                (progn (goto-char pos)
                       (narrow-to-region (point-min) limit)
                       (amos/subword-forward-internal))
              (goto-char (1+ pos))
              (narrow-to-region limit (point-max))
              (amos/subword-backward-internal))
            (point))))))

  (defun amos/word-movement-internal (subword dir)
    (let ((find-word-boundary-function-table
           (if subword
               amos/subword-find-word-boundary-function-table
             amos/subword-empty-char-table)))
      (let ((word-separating-categories evil-cjk-word-separating-categories)
            (word-combining-categories evil-cjk-word-combining-categories))
        (when subword (push '(?u . ?U) word-separating-categories))
        (forward-word dir))))

  ;; M-D forward-kill-subword (camelCase aware)
  (evil-define-command amos/delete-forward-subword ()
    (evil-signal-at-bob-or-eob 1)
    (amos/ensure-insert-state)
    (amos/with-private-kill-ring
     (kill-region (point)
                  (max (save-excursion
                         (if (looking-at "[ \t\r\n\v\f]")
                             (progn (re-search-forward "[^ \t\r\n\v\f]")
                                    (backward-char))
                           (amos/word-movement-internal t 1))
                         (point))
                       (line-beginning-position)))))

  ;; M-S-DEL backward-kill-subword (camelCase aware)
  (evil-define-command amos/delete-backward-subword ()
    (evil-signal-at-bob-or-eob -1)
    (unless (or (eolp) (evil-insert-state-p) (active-minibuffer-window))
      (amos/ensure-insert-state)
      (forward-char))
    (amos/with-private-kill-ring
     (kill-region (point)
                  (min (save-excursion
                         (if (looking-back "[ \t\r\n\v\f]" nil)
                             (progn (re-search-backward "[^ \t\r\n\v\f]")
                                    (forward-char))
                           (amos/word-movement-internal t -1))
                         (point))
                       (line-end-position)))))

  ;; M-f/M-b — forward/backward word movement (enters insert state from normal)
  (evil-define-command amos/forward-word-insert (&optional subword)
    (evil-signal-at-bob-or-eob 1)
    (amos/ensure-insert-state)
    (if (looking-at "[ \t\r\n\v\f]")
        (progn (re-search-forward "[^ \t\r\n\v\f]") (backward-char))
      (amos/word-movement-internal subword 1)))
  (evil-define-command amos/forward-subword-insert ()
    (amos/forward-word-insert t))
  (evil-define-command amos/backward-word-insert (&optional subword)
    (evil-signal-at-bob-or-eob -1)
    (unless (or (eolp) (evil-insert-state-p))
      (amos/ensure-insert-state)
      (forward-char))
    (if (looking-back "[ \t\r\n\v\f]" nil)
        (progn (re-search-backward "[^ \t\r\n\v\f]") (forward-char))
      (amos/word-movement-internal subword -1)))
  (evil-define-command amos/backward-subword-insert ()
    (amos/backward-word-insert t))

  ;; Key bindings — bound in both normal and insert; normal auto-enters insert
  (dolist (map (list evil-normal-state-map evil-insert-state-map))
    (define-key map (kbd "M-d") #'amos/delete-forward-word)
    (define-key map (kbd "M-D") #'amos/delete-forward-subword)
    (define-key map (kbd "M-f") #'amos/forward-word-insert)
    (define-key map (kbd "M-F") #'amos/forward-subword-insert)
    (define-key map (kbd "M-b") #'amos/backward-word-insert)
    (define-key map (kbd "M-DEL") #'amos/delete-backward-word)
    (define-key map (kbd "M-<backspace>") #'amos/delete-backward-word)
    (define-key map [134217855] #'amos/delete-backward-word)) ; M-DEL terminal encoding
  (define-key evil-insert-state-map (kbd "C-w") #'amos/delete-backward-word)
  (define-key evil-insert-state-map (kbd "DEL") #'amos/delete-backward-char)
  (define-key evil-insert-state-map (kbd "C-d") #'amos/delete-char)
  (define-key evil-insert-state-map (kbd "C-k") #'amos/kill-line)

  ;; Clipboard — GUI uses xclip, terminal uses OSC 52 escape sequences
  (defun amos/osc52-select-text (text &rest _)
    "Send TEXT to terminal clipboard via OSC 52 escape sequence."
    (let ((b64-length (+ (* (length text) 3) 2)))
      (if (<= b64-length 100000)
          (let* ((b64 (base64-encode-string
                       (encode-coding-string text 'utf-8) t))
                 (osc (concat "\e]52;c;" b64 "\07"))
                 (escaped (if (getenv "TMUX")
                              (concat "\033Ptmux;\033" osc "\033\\")
                            osc)))
            (send-string-to-terminal escaped))
        (message "Selection too long for OSC 52 (%d bytes)" b64-length))))

  (defun amos/x-select-text (text &rest _)
    "Copy TEXT to X11 clipboard via xclip."
    (with-temp-buffer
      (insert text)
      (call-process-region (point-min) (point-max)
                           "xclip" nil nil nil "-selection" "clipboard")))

  (if (display-graphic-p)
      (setq interprogram-cut-function #'amos/x-select-text)
    (setq interprogram-cut-function #'amos/osc52-select-text)))

;; Terminal cursor shape+color follows Evil state
;; Uses amosbird/evil-terminal-cursor-changer fork (supports OSC 12 color)
(use-package evil-terminal-cursor-changer
  :unless (display-graphic-p)
  :ensure nil
  :load-path "lisp/"
  :after evil
  :config (etcc-on))

(use-package evil-collection
  :after evil
  :custom
  (evil-collection-setup-minibuffer nil) ; Don't override minibuffer
  :config
  (evil-collection-init))

;; evil-visualstar — * and # in visual state search for the selection
(use-package evil-visualstar
  :after evil
  :config
  (evil-define-key* 'visual 'global
    "*" #'evil-visualstar/begin-search-forward
    "#" #'evil-visualstar/begin-search-backward))

;; evil-multiedit — edit multiple matches simultaneously (like VS Code Ctrl-D)
;; M-n selects current symbol and jumps to next match, M-p to previous
;; R in visual mode selects all matches
(use-package evil-multiedit
  :after evil
  :config
  (setq evil-multiedit-follow-matches t) ; Cursor follows matches
  ;; Normal: M-n/M-p match by symbol
  (define-key evil-normal-state-map (kbd "M-n") #'evil-multiedit-match-symbol-and-next)
  (define-key evil-normal-state-map (kbd "M-p") #'evil-multiedit-match-symbol-and-prev)
  ;; Normal: R — match all occurrences of word and hide unmatched lines
  (defun amos/multiedit-match-all-and-focus ()
    "Match all occurrences of symbol at point and hide unmatched lines."
    (interactive)
    (evil-multiedit-match-all)
    (iedit-show/hide-unmatched-lines))
  (define-key evil-normal-state-map (kbd "R") #'amos/multiedit-match-all-and-focus)
  ;; Normal: % — match all occurrences of word at point (like anzu-multiedit)
  (defun amos/multiedit-symbol ()
    "Enter multiedit for all occurrences of the symbol at point."
    (interactive)
    (let ((string (evil-find-thing t 'symbol)))
      (if (null string)
          (user-error "No symbol under point")
        (let* ((regex (format "\\_<%s\\_>" (regexp-quote string)))
               (search-pattern (evil-ex-make-search-pattern regex)))
          (evil-multiedit-ex-match (point-min) (point-max) nil (car search-pattern))))))
  (define-key evil-normal-state-map (kbd "%") #'amos/multiedit-symbol)
  ;; Visual: M-n/M-p match by selection, R matches all
  (define-key evil-visual-state-map (kbd "M-n") #'evil-multiedit-match-and-next)
  (define-key evil-visual-state-map (kbd "M-p") #'evil-multiedit-match-and-prev)
  (define-key evil-visual-state-map (kbd "R")   #'evil-multiedit-match-all)
  ;; Multiedit mode: j/k navigate, RET toggle, DEL exclude
  ;; Must use evil-define-key* to bind in evil's normal state
  (evil-define-key* 'normal evil-multiedit-mode-map
    "j"   #'evil-multiedit-next
    "k"   #'evil-multiedit-prev
    "RET" (lambda () (interactive)
            (evil-multiedit-toggle-or-restrict-region)
            (evil-multiedit-next))
    (kbd "DEL") (lambda () (interactive)
                  (evil-multiedit-toggle-or-restrict-region)
                  (evil-multiedit-prev))
    (kbd "C-f") #'iedit-show/hide-context-lines)
  ;; Cursor color
  (setq evil-multiedit-state-cursor  '("#ff3030" box))
  (setq evil-multiedit-insert-state-cursor '("#ff3030" bar))
  ;; Undo integration — merge multiedit-insert operations into a single undo step
  (add-hook 'evil-multiedit-insert-state-entry-hook
            (lambda () (evil-start-undo-step) (setq evil-in-single-undo t)))
  (add-hook 'evil-multiedit-insert-state-exit-hook
            (lambda () (setq evil-in-single-undo nil) (evil-end-undo-step))))

;; evil-surround — cs/ds/ys for surround pairs (quotes/parens/tags)
;; cs"' changes double quotes to single, ds( deletes parens, ysiw" wraps word in quotes
(use-package evil-surround
  :after evil
  :config (global-evil-surround-mode 1))

;; move-text — move lines/regions up and down
(use-package move-text
  :commands (move-text-up move-text-down))

;; smartparens — structural editing (slurp/barf s-expressions)
;; M-r absorbs the next expression into the current sexp (slurp)
;; M-R pushes the last expression out of the current sexp (barf)
(use-package smartparens
  :hook (prog-mode . smartparens-mode)
  :config
  (require 'smartparens-config)
  (sp-pair "{" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "(" nil :post-handlers '(("||\n[i]" "RET") ("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (sp-pair "[" nil :post-handlers '(("| " " "))
           :unless '(sp-point-before-word-p sp-point-before-same-p))
  (define-key evil-insert-state-map (kbd "M-r") #'sp-slurp-hybrid-sexp)
  (define-key evil-insert-state-map (kbd "M-R") #'sp-forward-barf-sexp))

;; surround-with-pair — M-( M-) M-{ M-} etc. in insert mode
(with-eval-after-load 'evil-surround
  (defun amos/surround-with-pair (c &optional back)
    (let* ((e (save-excursion
                (if back
                    (amos/backward-word-insert)
                  (amos/forward-word-insert))
                (point)))
           (b (point)))
      (if (< b e)
          (evil-surround-region b e t c)
        (save-excursion
          (evil-surround-region e b t c))
        (forward-char 1))))
  (define-key evil-insert-state-map (kbd "M-{") (lambda () (interactive) (amos/surround-with-pair ?} t)))
  (define-key evil-insert-state-map (kbd "M-}") (lambda () (interactive) (amos/surround-with-pair ?})))
  (define-key evil-insert-state-map (kbd "M-(") (lambda () (interactive) (amos/surround-with-pair ?\) t)))
  (define-key evil-insert-state-map (kbd "M-)") (lambda () (interactive) (amos/surround-with-pair ?\))))
  (define-key evil-insert-state-map (kbd "M-,") (lambda () (interactive) (amos/surround-with-pair ?> t)))
  (define-key evil-insert-state-map (kbd "M-.") (lambda () (interactive) (amos/surround-with-pair ?>)))
  (define-key evil-insert-state-map (kbd "M-'") (lambda () (interactive) (amos/surround-with-pair ?\')))
  (define-key evil-insert-state-map (kbd "M-_") (lambda () (interactive) (amos/surround-with-pair ?\' t)))
  (define-key evil-insert-state-map (kbd "M-\"") (lambda () (interactive) (amos/surround-with-pair ?\")))
  (define-key evil-insert-state-map (kbd "M-p") (lambda () (interactive) (amos/surround-with-pair ?\" t))))

;; evil-exchange — gx to swap two text regions
(use-package evil-exchange
  :after evil
  :config (evil-exchange-install))

;; evil-nerd-commenter — gl to comment/uncomment, gy to copy and comment
(use-package evil-nerd-commenter
  :after evil
  :config
  (define-key evil-normal-state-map "gl" #'evilnc-comment-or-uncomment-lines)
  (define-key evil-visual-state-map "gl" #'evilnc-comment-or-uncomment-lines)
  (define-key evil-normal-state-map "gy" #'evilnc-copy-and-comment-lines)
  (define-key evil-visual-state-map "gy" #'evilnc-copy-and-comment-lines))

;; evil-inner-arg — ia/aa function argument text objects
(use-package evil-args
  :after evil
  :config
  (define-key evil-inner-text-objects-map "a" #'evil-inner-arg)
  (define-key evil-outer-text-objects-map "a" #'evil-outer-arg))

;; evil-indent-plus — ii/iI/iJ indentation text objects
(use-package evil-indent-plus
  :after evil
  :config
  (define-key evil-inner-text-objects-map "i" #'evil-indent-plus-i-indent)
  (define-key evil-outer-text-objects-map "i" #'evil-indent-plus-a-indent)
  (define-key evil-inner-text-objects-map "I" #'evil-indent-plus-i-indent-up)
  (define-key evil-outer-text-objects-map "I" #'evil-indent-plus-a-indent-up)
  (define-key evil-inner-text-objects-map "J" #'evil-indent-plus-i-indent-up-down)
  (define-key evil-outer-text-objects-map "J" #'evil-indent-plus-a-indent-up-down))

;; evil-textobj-line — il/al line text objects
(use-package evil-textobj-line :after evil)

;; ij/aj — any surrounding object: find tightest enclosing delimiter
;; Tries all evil built-in text objects and picks the smallest range.
(with-eval-after-load 'evil
  (defun amos/try-text-object (obj-fn count type)
    "Try to get range from OBJ-FN, return (beg . end) or nil."
    (condition-case nil
        (let ((range (funcall obj-fn count nil nil type)))
          (when (and range (evil-range-p range))
            (let ((b (evil-range-beginning range))
                  (e (evil-range-end range)))
              (when (and b e (< b e) (<= b (point)) (>= e (point)))
                (cons b e)))))
      (error nil)))

  (defun amos/tightest-object (obj-fns count type)
    "Among OBJ-FNS, return the evil-range with smallest span containing point."
    (let (best best-size)
      (dolist (fn obj-fns)
        (let ((r (amos/try-text-object fn count type)))
          (when r
            (let ((size (- (cdr r) (car r))))
              (when (or (null best) (< size best-size))
                (setq best r best-size size))))))
      (when best
        (evil-range (car best) (cdr best) type :expanded t))))

  (evil-define-text-object amos/any-object-inner (count &optional beg end type)
    "Select inside the tightest surrounding delimiter."
    (amos/tightest-object
     '(evil-inner-paren evil-inner-bracket evil-inner-curly
       evil-inner-angle evil-inner-single-quote evil-inner-double-quote
       evil-inner-back-quote)
     count type))

  (evil-define-text-object amos/any-object-outer (count &optional beg end type)
    "Select around the tightest surrounding delimiter."
    (amos/tightest-object
     '(evil-a-paren evil-a-bracket evil-a-curly
       evil-an-angle evil-a-single-quote evil-a-double-quote
       evil-a-back-quote)
     count type))

  (define-key evil-inner-text-objects-map "j" #'amos/any-object-inner)
  (define-key evil-outer-text-objects-map "j" #'amos/any-object-outer))

;; evil-numbers — C-a/C-q to increment/decrement numbers
(use-package evil-numbers
  :after evil
  :config
  (define-key evil-normal-state-map (kbd "C-a") #'evil-numbers/inc-at-pt)
  (define-key evil-normal-state-map (kbd "C-q") #'evil-numbers/dec-at-pt)
  (define-key evil-visual-state-map (kbd "C-a") #'evil-numbers/inc-at-pt)
  (define-key evil-visual-state-map (kbd "C-q") #'evil-numbers/dec-at-pt)
  (define-key evil-visual-state-map (kbd "g C-a") #'evil-numbers/inc-at-pt-incremental)
  (define-key evil-visual-state-map (kbd "g C-q") #'evil-numbers/dec-at-pt-incremental))

;; rotate-text — ! toggles true/false, yes/no, etc.
(use-package rotate-text
  :ensure nil
  :load-path "lisp/"
  :after evil
  :config
  (define-key evil-normal-state-map "!" #'rotate-text))

;; expand-region — visual v to expand, V to contract selection
(with-eval-after-load 'expand-region
  (define-key evil-visual-state-map "v" 'er/expand-region)
  (define-key evil-visual-state-map "V" 'er/contract-region))

;; goto-last-change — g. jump to last edit position
(use-package goto-last-change
  :after evil
  :config
  (define-key evil-normal-state-map "g." #'goto-last-change))

;; Evil extra normal state bindings
(with-eval-after-load 'evil
  ;; go/gO — insert blank line below/above (without entering insert)
  (defun amos/evil-insert-line-below (count)
    (interactive "p")
    (dotimes (_ count) (save-excursion (evil-insert-newline-below))))
  (defun amos/evil-insert-line-above (count)
    (interactive "p")
    (dotimes (_ count) (save-excursion (evil-insert-newline-above))))
  (define-key evil-normal-state-map "go" #'amos/evil-insert-line-below)
  (define-key evil-normal-state-map "gO" #'amos/evil-insert-line-above)

  ;; gp — reselect last pasted text
  (defun amos/reselect-paste ()
    (interactive)
    (let ((start (evil-get-marker ?\[))
          (end (evil-get-marker ?\])))
      (when (and start end)
        (evil-visual-select start end))))
  (define-key evil-normal-state-map "gp" #'amos/reselect-paste)

  ;; 0 — toggle between BOL and first non-blank character
  (defun amos/toggle-bol ()
    (interactive)
    (let ((col (current-column)))
      (back-to-indentation)
      (when (= col (current-column))
        (evil-beginning-of-line))))
  (define-key evil-normal-state-map "0" #'amos/toggle-bol)
  (define-key evil-visual-state-map "0" #'amos/toggle-bol)

  ;; - → end of line
  (define-key evil-normal-state-map "-" #'evil-end-of-line)
  (define-key evil-visual-state-map "-" #'evil-end-of-line)

  ;; C-j/C-k — move line up/down (autoloaded via use-package below)
  (define-key evil-normal-state-map (kbd "C-j") #'move-text-down)
  (define-key evil-normal-state-map (kbd "C-k") #'move-text-up)
  (define-key evil-visual-state-map (kbd "C-j") #'move-text-down)
  (define-key evil-visual-state-map (kbd "C-k") #'move-text-up)

  ;; < > keep visual selection after indent
  (defun amos/visual-indent ()
    (interactive)
    (evil-shift-right (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))
  (defun amos/visual-dedent ()
    (interactive)
    (evil-shift-left (region-beginning) (region-end))
    (evil-normal-state)
    (evil-visual-restore))
  (define-key evil-visual-state-map ">" #'amos/visual-indent)
  (define-key evil-visual-state-map "<" #'amos/visual-dedent)

  ;; == reformat current line/region via eglot (LSP), fallback to indent
  (defun amos/evil-indent-advice (orig-fn beg end)
    "Use eglot range formatting for == when LSP is active."
    (if (and (bound-and-true-p eglot--managed-mode)
             (eglot-current-server))
        (eglot-format beg end)
      (funcall orig-fn beg end)))
  (advice-add #'evil-indent :around #'amos/evil-indent-advice)

  ;; C-e — end-of-statement: add ; at EOL for C-like languages, or move to EOL
  (defvar amos/end-of-statement-alist nil
    "Alist of (MODE . (:regex-char (TRAILING-WS-RE SKIP-CHARS CHAR))).
TRAILING-WS-RE: if EOL matches this, just clean whitespace.
SKIP-CHARS: if last char matches, line already has a terminator.
CHAR: character to insert as statement terminator.")
  (dolist (mode '(c-mode c++-mode c-ts-mode c++-ts-mode java-mode
                  js-mode js-ts-mode typescript-mode typescript-ts-mode))
    (push (cons mode '(:regex-char ("[ \t\r\n\v\f]" "[[{(;]" ?\;)))
          amos/end-of-statement-alist))
  (push (cons 'sql-mode '(:regex-char ("[ \t\r\n\v\f]" ";" ?\;)))
        amos/end-of-statement-alist)
  (push (cons 'emacs-lisp-mode '(:regex-char "[ \t\r\n\v\f]"))
        amos/end-of-statement-alist)

  (defun amos/maybe-add-end-of-statement (&optional move)
    "Add end-of-statement char (;) at EOL if missing, or move to EOL.
With MOVE non-nil, also move point to EOL."
    (interactive)
    (if (and move (not (eolp)))
        (end-of-line)
      (let ((p (save-excursion
                 (let (s e)
                   (back-to-indentation) (setq s (point))
                   (end-of-line) (setq e (point))
                   (when-let* ((plist (cdr (assq major-mode amos/end-of-statement-alist))))
                     (when-let* ((rc (plist-get plist :regex-char))
                                 (regex-char (if (listp rc) rc (list rc))))
                       (if (looking-back (car regex-char) nil)
                           (delete-trailing-whitespace s e)
                         (when (and (nth 1 regex-char) (nth 2 regex-char))
                           (if (looking-back (nth 1 regex-char) 1)
                               (when move
                                 (funcall-interactively (key-binding (kbd "RET"))))
                             (insert (nth 2 regex-char)))))))
                   (point)))))
        (when move (goto-char p)))))

  (define-key evil-normal-state-map (kbd "C-e") #'amos/maybe-add-end-of-statement)
  (define-key evil-insert-state-map (kbd "C-e")
              (lambda () (interactive) (amos/maybe-add-end-of-statement t)))

  ;; visual s → surround
  (define-key evil-visual-state-map "s" #'evil-surround-region)

  ;; ,, — switch between header/source (C/C++) or find other file
  (defun amos/find-other-file ()
    "Switch between related files smartly.
For C/C++: use clangd switchSourceHeader if LSP available, else ff-find-other-file.
For cc-playground: switch between src and test.
Other modes: project-find-file."
    (interactive)
    (cond
     ;; cc-playground: src <-> test
     ((and (boundp 'cc-playground-mode) cc-playground-mode)
      (cc-switch-between-src-and-test))
     ;; C/C++: try clangd LSP, fallback to ff-find-other-file
     ((memq major-mode '(c-mode c++-mode c-ts-mode c++-ts-mode objc-mode))
      (if (and (bound-and-true-p eglot--managed-mode)
               (eglot-current-server))
          (condition-case _err
              (let ((result (jsonrpc-request
                             (eglot-current-server)
                             :textDocument/switchSourceHeader
                             (eglot--TextDocumentIdentifier))))
                (if (and result (not (string-empty-p result)))
                    (find-file (eglot--uri-to-path result))
                  (ff-find-other-file nil t)))
            (error (ff-find-other-file nil t)))
        (ff-find-other-file nil t)))
     ;; Fallback: find file in project
     (t (project-find-file))))
  (define-key evil-normal-state-map ",," #'amos/find-other-file)

  ;; K/L — paste pop (cycle through kill-ring)
  (define-key evil-normal-state-map "K" #'evil-paste-pop)
  (define-key evil-normal-state-map "L" #'evil-paste-pop-next)

  ;; Visual paste: always delete selection to black hole register so the
  ;; yanked content stays available for repeated pastes (Doom behavior).
  (evil-define-command amos/evil-visual-paste (count &optional register)
    "Paste over Visual selection, preserving yanked text."
    :suppress-operator t
    (interactive "P<x>")
    (setq this-command 'evil-visual-paste)
    (let* ((text (if register
                     (evil-get-register register)
                   (current-kill 0)))
           (yank-handler (car-safe (get-text-property
                                    0 'yank-handler text)))
           new-kill paste-eob)
      (evil-with-undo
        (let* ((kill-ring (list (current-kill 0)))
               (kill-ring-yank-pointer kill-ring))
          (when (evil-visual-state-p)
            (evil-visual-rotate 'upper-left)
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
        (setq evil-last-paste
              (list (nth 0 evil-last-paste)
                    (nth 1 evil-last-paste)
                    (nth 2 evil-last-paste)
                    (nth 3 evil-last-paste)
                    (nth 4 evil-last-paste)
                    t)))))
  (advice-add #'evil-visual-paste :override #'amos/evil-visual-paste)

  ;; Make terminal paste (xterm-paste) replace selection in non-insert
  ;; evil states: visual mode uses evil-visual-paste, multiedit normal
  ;; mode uses evil-multiedit--paste-replace, regular normal mode uses
  ;; evil-paste-after.  Insert mode keeps the default xterm-paste
  ;; behavior (plain insert).
  (defun amos/xterm-paste-with-evil-replace (event)
    "Terminal paste that respects evil state and replaces selections."
    (interactive "e")
    (let ((text (nth 1 event)))
      (kill-new text)
      (cond
       ((evil-visual-state-p)
        (evil-visual-paste 1))
       ((and (bound-and-true-p evil-multiedit-mode)
             (not (evil-insert-state-p)))
        (evil-multiedit--paste-replace))
       ((evil-normal-state-p)
        (evil-paste-after 1))
       (t
        ;; Insert state or other: just insert
        (insert-for-yank text)))))

  (define-key evil-normal-state-map [xterm-paste]
    #'amos/xterm-paste-with-evil-replace)
  (define-key evil-visual-state-map [xterm-paste]
    #'amos/xterm-paste-with-evil-replace)
  (with-eval-after-load 'evil-multiedit
    (evil-define-key* '(normal visual) evil-multiedit-mode-map
      [xterm-paste] #'amos/xterm-paste-with-evil-replace))

  ;; M-s/M-S — quick substitute (evil ex)
  (evil-define-command amos/line-substitute () (evil-ex "s/"))
  (evil-define-command amos/all-substitute () (evil-ex "%s/"))
  (evil-define-command amos/region-substitute () (evil-ex "'<,'>s/"))
  (define-key evil-normal-state-map (kbd "M-s") #'amos/line-substitute)
  (define-key evil-normal-state-map (kbd "M-S") #'amos/all-substitute)
  (define-key evil-visual-state-map (kbd "M-s") #'amos/region-substitute)

  ;; ── Escape handler (replaces Doom's doom/escape) ──
  ;; ESC in normal state: save buffer, update git-gutter, close side windows.
  ;; Side windows are detected by the 'window-side parameter set by
  ;; display-buffer-in-side-window (our display-buffer-alist rules).

  (defun amos/escape (&optional interactive)
    "Run escape actions: close side windows, save buffer, refresh git-gutter.
Designed to be called from evil normal state entry and also bound to ESC
in normal state for a second press."
    (interactive (list 'interactive))
    (cond
     ;; If we ARE in a side window, just quit it
     ((window-parameter nil 'window-side)
      (quit-window))
     ;; Otherwise, try to close any visible side windows remotely
     (t
      (let (closed)
        (dolist (win (window-list))
          (when (and (window-live-p win)
                     (window-parameter win 'window-side))
            (ignore-errors (delete-window win))
            (setq closed t)))
        closed))))

  ;; Hook: on ESC (entering normal state from insert/visual), only close side windows.
  ;; Save is intentionally NOT done here — user expects save only on explicit ESC in normal state.
  (defun amos/on-escape-to-normal ()
    "Close side windows when transitioning from insert/visual to normal."
    (when (memq evil-previous-state '(insert visual replace operator))
      (amos/escape)))
  (add-hook 'evil-normal-state-entry-hook #'amos/on-escape-to-normal)

  ;; ESC pressed while already in normal state — save buffer + close side windows
  (defun amos/escape-in-normal ()
    "Save buffer, clear search highlights, and close side windows on ESC in normal state."
    (interactive)
    ;; Abort evil-multiedit first; if active, just exit multiedit and stop
    (if (and (bound-and-true-p evil-multiedit-mode)
             (ignore-errors (evil-multiedit-abort) t))
        nil ; multiedit aborted, done
      (when (evil-ex-hl-active-p 'evil-ex-search)
        (evil-ex-nohighlight))
      (when (and (buffer-file-name)
                 (not defining-kbd-macro)
                 (buffer-modified-p))
        (save-buffer))
      (amos/escape)))
  (define-key evil-normal-state-map [escape] #'amos/escape-in-normal)

  ;; C-l — redraw display and recenter (clear artifacts, reset hscroll)
  (defun amos/redisplay-and-recenter (&rest _)
    (interactive)
    (when (> (window-hscroll) 0)
      (evil-scroll-left 10))
    (redraw-display)
    (ignore-errors (recenter)))
  (define-key evil-normal-state-map (kbd "C-l") #'amos/redisplay-and-recenter)

  ;; C-l in insert state — expand yasnippet, fallback to completion
  (define-key evil-insert-state-map (kbd "C-l") #'amos/expand-or-complete)
  (defun amos/expand-or-complete ()
    "Try to expand yasnippet, fallback to completion-at-point."
    (interactive)
    (if (and (bound-and-true-p yas-minor-mode)
             (yas-maybe-expand-abbrev-key-filter 'yas-expand))
        (yas-expand)
      (completion-at-point)))

  ;; C-y — copy filename:line to kill-ring
  (define-key evil-normal-state-map (kbd "C-y") #'amos/yank-buffer-filename-with-line-position)

  ;; M-h/M-l — quickly switch to left/right window
  (define-key evil-normal-state-map (kbd "M-h") #'evil-window-left)
  (define-key evil-normal-state-map (kbd "M-l") #'evil-window-right)
  (define-key evil-insert-state-map (kbd "M-h") #'evil-window-left)
  (define-key evil-insert-state-map (kbd "M-l") #'evil-window-right)

  ;; M-j/M-k — jump to error if grep buffer is visible, otherwise switch up/down window
  (defun amos/goto-error (&optional prev)
    (catch 'return
      (dolist (win (window-list-1))
        (with-selected-window win
          (with-current-buffer (window-buffer win)
            (when (eq major-mode 'grep-mode)
              (let ((inhibit-message t))
                (if prev (previous-error) (next-error)))
              (throw 'return nil)))))
      (if prev (windmove-up) (windmove-down))))
  (defun amos/goto-next-error () (interactive) (amos/goto-error))
  (defun amos/goto-previous-error () (interactive) (amos/goto-error t))
  (dolist (map (list evil-normal-state-map evil-insert-state-map
                     evil-visual-state-map evil-motion-state-map))
    (define-key map (kbd "M-j") #'amos/goto-next-error)
    (define-key map (kbd "M-k") #'amos/goto-previous-error))

  ;; C-w in normal mode buries buffer (without closing window)
  (defun amos/close-current-buffer ()
    (interactive)
    (bury-buffer))
  (define-key evil-normal-state-map (kbd "C-w") #'amos/close-current-buffer)

  ;; M-m — toggle between current and last buffer
  (defun amos/switch-buffer ()
    "Switch to the most recent other buffer."
    (interactive)
    (switch-to-buffer (other-buffer (current-buffer) t)))
  (dolist (map (list evil-normal-state-map evil-insert-state-map
                     evil-visual-state-map evil-motion-state-map))
    (define-key map (kbd "M-m") #'amos/switch-buffer))

  ;; M-W — kill current buffer, M-w — wipe (kill without saving)
  (defun amos/kill-current-buffer ()
    "Kill the current buffer."
    (interactive)
    (kill-current-buffer))
  (defun amos/wipe-current-buffer ()
    "Kill the current buffer without saving."
    (interactive)
    (set-buffer-modified-p nil)
    (kill-current-buffer))
  (define-key evil-normal-state-map (kbd "M-W") #'amos/kill-current-buffer)
  (define-key evil-normal-state-map (kbd "M-w") #'amos/wipe-current-buffer)

  ;; gd — go to definition, gr — find references, gf — find file at point
  (define-key evil-normal-state-map "gd" #'xref-find-definitions)
  (define-key evil-normal-state-map "gr" #'xref-find-references)
  (define-key evil-normal-state-map "gf" #'find-file-at-point)

  ;; C-n/C-p — next/prev line in insert state
  (define-key evil-insert-state-map (kbd "C-n") #'next-line)
  (define-key evil-insert-state-map (kbd "C-p") #'previous-line)
  ;; C-a — beginning of line in insert state
  (define-key evil-insert-state-map (kbd "C-a") #'evil-beginning-of-line)
  ;; C-u — backward kill to BOL in insert state
  (define-key evil-insert-state-map (kbd "C-u") #'amos/backward-kill-to-bol)
  ;; M-i — insert snippet in insert state
  (define-key evil-insert-state-map (kbd "M-i") #'yas-insert-snippet)
  ;; M-y — yank-pop in insert state
  (define-key evil-insert-state-map (kbd "M-y") #'yank-pop)
  ;; M-y — yank flymake error in normal state
  (define-key evil-normal-state-map (kbd "M-y") #'amos/yank-flymake-error)

  ;; N — search previous (motion state)
  (define-key evil-motion-state-map "N" #'evil-ex-search-previous)

  ;; C-x extras
  (global-set-key (kbd "C-x e") #'pp-eval-last-sexp)
  (global-set-key (kbd "C-x C-c") #'amos/tmux-detach)
  (global-set-key (kbd "C-x r") #'amos/tmux-source)

  ;; F1/F2/F3 — zoom controls
  (defun amos/reset-zoom () (interactive) (text-scale-set 0))
  (defun amos/increase-zoom () (interactive) (text-scale-increase 0.5))
  (defun amos/decrease-zoom () (interactive) (text-scale-decrease 0.5))
  (global-set-key (kbd "<f1>") #'amos/reset-zoom)
  (global-set-key (kbd "<f2>") #'amos/decrease-zoom)
  (global-set-key (kbd "<f3>") #'amos/increase-zoom)

  ;; zygospore — C-x 1 toggles delete-other-windows / restore window layout
  (defvar amos/zygospore-register "zygospore-win"
    "Register name for saving window config.")
  (defvar amos/zygospore-last-window nil)
  (defvar amos/zygospore-last-buffer nil)
  (defun amos/zygospore-toggle ()
    "Toggle between single window and previous multi-window layout."
    (interactive)
    (if (and (equal (selected-window) (next-window))
             (equal (selected-window) amos/zygospore-last-window)
             (equal (current-buffer) amos/zygospore-last-buffer))
        (jump-to-register amos/zygospore-register)
      (setq amos/zygospore-last-window (selected-window))
      (setq amos/zygospore-last-buffer (current-buffer))
      (window-configuration-to-register amos/zygospore-register)
      (delete-other-windows)))
  (global-set-key (kbd "C-x 1") #'amos/zygospore-toggle)
  (global-set-key (kbd "C-x C-1") #'amos/zygospore-toggle))

;; ── Evil repeat ignore list ──
;; Prevent navigation, UI, and side-effect-free commands from polluting evil's
;; "." repeat.  Only editing commands should be repeatable.
(defun amos/ignore-repeat (&rest prefixes)
  "Mark all commands matching any of PREFIXES as ignored by evil repeat."
  (dolist (prefix prefixes)
    (dolist (cmd (all-completions prefix obarray 'commandp))
      (evil-declare-ignore-repeat (intern cmd)))))

;; Commands defined in this config
(amos/ignore-repeat
 "amos/all-substitute"
 "amos/close-current-buffer"
 "amos/consult"
 "amos/decrease-zoom"
 "amos/escape"
 "amos/find-other-file"
 "amos/flymake"
 "amos/format-region-or-buffer"
 "amos/goto"
 "amos/increase-zoom"
 "amos/kill-current-buffer"
 "amos/kitty"
 "amos/launch"
 "amos/line-substitute"
 "amos/maybe-add-end-of-statement"
 "amos/pulse-line"
 "amos/redisplay-and-recenter"
 "amos/region-substitute"
 "amos/rename-current-buffer-file"
 "amos/reset-zoom"
 "amos/revert"
 "amos/run-script"
 "amos/shell-command"
 "amos/switch-buffer"
 "amos/tmux"
 "amos/wipe-current-buffer"
 "amos/workspace"
 "amos/xterm-paste"
 "amos/yank-buffer-filename"
 "amos/yank-flymake-error"
 "amos/zygospore"
 ;; Built-in / third-party commands
 "execute-extended-command"
 "find-file"
 "eval-defun"
 "pp-eval-last-sexp"
 "save-buffer"
 "split-window"
 "switch-to-buffer"
 "toggle-truncate-lines"
 "undo-tree"
 "vc-revert"
 "whitespace-mode"
 "move-text"
 "evil-commentary-line"
 "evil-multiedit"
 "evil-next-line"
 "evil-previous-line"
 "evil-ex-search"
 "evil-window"
 )

;; Per-package ignore-repeat (applied when each package loads)
(with-eval-after-load 'consult   (amos/ignore-repeat "consult"))
(with-eval-after-load 'dired     (amos/ignore-repeat "dired"))
(with-eval-after-load 'flymake   (amos/ignore-repeat "flymake"))
(with-eval-after-load 'git-gutter (amos/ignore-repeat "git-gutter"))
(with-eval-after-load 'git-timemachine (amos/ignore-repeat "git-timemachine"))
(with-eval-after-load 'magit     (amos/ignore-repeat "magit"))
(with-eval-after-load 'eglot     (amos/ignore-repeat "eglot"))
(with-eval-after-load 'envrc     (amos/ignore-repeat "envrc"))
(with-eval-after-load 'editorconfig (amos/ignore-repeat "editorconfig"))

;; Explicitly mark completion as change-repeat (so "." replays the completion)
(mapc #'evil-declare-change-repeat '(amos/expand-or-complete))

;; M-g jump to directory — integrates with shell jump tool (evil normal, bound after consult loads)

;; general.el — SPC leader key definitions
(use-package general
  :demand t
  :config
  (general-create-definer amos/leader
    :states '(normal visual motion)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "M-SPC"))

;; ============================================================================
;; §3 Editing Enhancements
;; ============================================================================

;;;; electric-pair — auto-close parens/quotes (built-in, lightweight smartparens alternative)
(add-hook 'after-init-hook #'electric-pair-mode)

;;;; Paren match highlighting
(setq show-paren-delay 0.1
      show-paren-highlight-openparen t
      show-paren-when-point-inside-paren t)
(add-hook 'after-init-hook #'show-paren-mode)

;;;; vundo — visual undo tree, open with C-x u
(use-package vundo
  :bind ("C-x u" . vundo)
  :custom
  (vundo-glyph-alist vundo-unicode-symbols))

;;;; expand-region — C-= incrementally expand selection (word → sexp → block → …)
;; Also bound to v/V in evil visual state (see §2)
(use-package expand-region
  :bind ("C-=" . er/expand-region))

;;;; Delete trailing whitespace on save
(add-hook 'before-save-hook #'delete-trailing-whitespace)

;;;; Indentation — use spaces instead of tabs, width 4
(setq-default indent-tabs-mode nil
              tab-width 4)
(setq tab-always-indent 'complete)      ; TAB indents first, then completes

;;;; Wrap at 120 characters per line
(setq-default fill-column 120)

;;;; Require final newline (POSIX convention)
(setq require-final-newline t)

;;;; CJK-aware word wrapping to avoid awkward line breaks in mixed text
(setq-default truncate-lines nil)         ; Line wrap by default
(setq word-wrap-by-category t)

;;;; unfill — M-Q to unwrap paragraph to a single line (inverse of fill-paragraph)
(use-package unfill
  :bind ("M-Q" . unfill-paragraph))

;;;; Comment enhancements — support multiline and empty-line comments
(setq comment-multi-line t)
(setq comment-empty-lines t)

;;;; syntactic-close — C-RET smart-close parens/strings/comments
(use-package syntactic-close
  :bind ("C-<return>" . syntactic-close))

;;;; recentf — remember recently opened files (used by M-x consult-recent-file)
(use-package recentf
  :ensure nil
  :hook (after-init . recentf-mode)
  :custom
  (recentf-max-saved-items 300)
  (recentf-auto-cleanup 'mode))

;;;; savehist — persist minibuffer history (search, commands, etc. across sessions)
(use-package savehist
  :ensure nil
  :hook (after-init . savehist-mode)
  :custom
  (history-length 300)
  (savehist-save-minibuffer-history t)
  (savehist-additional-variables
   '(kill-ring register-alist mark-ring global-mark-ring
               search-ring regexp-search-ring)))

;;;; saveplace — restore cursor position when reopening files
(use-package saveplace
  :ensure nil
  :hook (after-init . save-place-mode))

;; ============================================================================
;; §4 Completion Framework
;; ============================================================================
;;
;; Modern Emacs completion stack (all based on the completing-read interface):
;;
;;   ┌─────────────────────────────────────────────┐
;;   │  Corfu      — In-buffer completion popup     │
;;   │  Cape       — Completion backends (dabbrev, file, etc.) │
;;   ├─────────────────────────────────────────────┤
;;   │  Vertico    — Vertical completion UI (minibuffer) │
;;   │  Orderless  — Fuzzy matching (space-separated keywords) │
;;   │  Marginalia — Annotations for candidates     │
;;   ├─────────────────────────────────────────────┤
;;   │  Consult    — Enhanced search/navigation commands │
;;   │  Embark     — Context actions on candidates  │
;;   └─────────────────────────────────────────────┘

;;;; Vertico — vertical completion UI, replacing the default horizontal completion
(use-package vertico
  :hook (after-init . vertico-mode)
  :custom
  (vertico-cycle t)                     ; Cycle back to top at bottom
  (vertico-count 20)                    ; Show 20 candidates
  (vertico-resize nil))                 ; Fixed height, no auto-resize

;; vertico-directory — DEL deletes entire path component in file path completion
(use-package vertico-directory
  :after vertico
  :ensure nil
  :bind (:map vertico-map
              ("RET"   . vertico-directory-enter)
              ("DEL"   . vertico-directory-delete-char)
              ("M-DEL" . vertico-directory-delete-word)
              ("C-j"   . vertico-next)
              ("C-k"   . vertico-previous)
              ("C-w"   . amos/consult-yank-word)
              ("TAB"   . amos/vertico-tab))
  :hook (rfn-eshadow-update-overlay . vertico-directory-tidy)
  :config
  (defun amos/vertico-tab ()
    "TAB in vertico: in consult sessions, do nothing (let post-command-hook
trigger preview). Otherwise call vertico-insert."
    (interactive)
    (unless (bound-and-true-p consult--preview-function)
      (vertico-insert))))

;; Minibuffer keybindings — match doom config for consistent editing experience
(dolist (map (list minibuffer-local-map
                   minibuffer-local-ns-map
                   minibuffer-local-completion-map
                   minibuffer-local-must-match-map
                   minibuffer-local-shell-command-map
                   read-expression-map))
  (define-key map [escape]        #'abort-recursive-edit)
  (define-key map (kbd "C-a")     #'move-beginning-of-line)
  (define-key map (kbd "C-b")     #'backward-char)
  (define-key map (kbd "C-f")     #'forward-char)
  (define-key map (kbd "C-d")     #'amos/delete-char)
  (define-key map (kbd "C-o")     #'amos/kill-line)
  (define-key map (kbd "C-u")     #'amos/backward-kill-to-bol)
  (define-key map (kbd "C-k")     #'previous-line-or-history-element)
  (define-key map (kbd "C-j")     #'next-line-or-history-element)
  (define-key map (kbd "C-n")     #'next-line-or-history-element)
  (define-key map (kbd "C-p")     #'previous-line-or-history-element)
  (define-key map (kbd "M-b")     #'amos/backward-word-insert)
  (define-key map (kbd "M-f")     #'amos/forward-word-insert)
  (define-key map (kbd "M-B")     #'amos/backward-subword-insert)
  (define-key map (kbd "M-F")     #'amos/forward-subword-insert)
  (define-key map (kbd "M-d")     #'amos/delete-forward-word)
  (define-key map (kbd "M-D")     #'amos/delete-forward-subword)
  (define-key map (kbd "M-z")     #'undo)
  (define-key map (kbd "DEL")     #'amos/delete-backward-char)
  (define-key map (kbd "M-DEL")   #'amos/delete-backward-word)
  (define-key map (kbd "M-<backspace>") #'amos/delete-backward-word)
  (define-key map [134217855]     #'amos/delete-backward-word))

;; consult yank-word — C-w pulls word at cursor into minibuffer search
(defun amos/consult-yank-word (&optional arg)
  "Pull next word from buffer into minibuffer search string."
  (interactive "p")
  (let (text)
    (with-minibuffer-selected-window
      (let ((beg (point))
            (bol (line-beginning-position))
            (eol (line-end-position))
            end)
        (unwind-protect
            (progn (forward-word arg)
                   (setq end (goto-char (max bol (min (point) eol))))
                   (setq text (buffer-substring-no-properties beg end)))
          (unless text (goto-char beg)))))
    (when text
      (insert (replace-regexp-in-string "  +" " " text t t)))))

;;;; Orderless — space-separated fuzzy matching
;; e.g., typing "buf sw" matches "switch-to-buffer"
(use-package orderless
  :custom
  (completion-styles '(orderless basic))
  (completion-category-overrides '((file (styles partial-completion)))))

;;;; Marginalia — show extra info alongside completion candidates
;; e.g., M-x shows keybindings and documentation summaries
(use-package marginalia
  :hook (after-init . marginalia-mode)
  :custom
  (marginalia-align 'right)             ; Right-align annotations for tidy layout
  (marginalia-field-width 80))          ; Annotation field width

;;;; Consult — enhanced search and navigation commands
(use-package consult
  :bind (("C-s"     . consult-line)
         ("C-x b"   . consult-buffer)
         ("M-s r"   . consult-ripgrep)
         ("M-s f"   . consult-fd)
         ("M-s l"   . consult-line)
         ("M-s L"   . consult-line-multi)
         ("M-y"     . consult-yank-pop)
         ("M-g g"   . consult-goto-line)
         ("M-g M-g" . consult-goto-line)
         ("M-g i"   . consult-imenu)
         ("M-g o"   . consult-outline)
         ("M-g m"   . consult-mark)
         ("M-g k"   . consult-global-mark)
         ("M-g f"   . consult-flymake))
  :custom
  (consult-narrow-key "<")              ; < to narrow candidate sources
  (consult-ripgrep-args                 ; ripgrep arguments
   "rg --null --line-buffered --color=never --max-columns=1000 --path-separator / --smart-case --no-heading --with-filename --line-number")
  :config
  (setq consult-preview-key "TAB")
  (add-to-list 'consult-preview-allowed-hooks 'global-hl-line-mode-check-buffers)
  ;; Route xref through consult for TAB preview support
  (setq xref-show-xrefs-function #'consult-xref
        xref-show-definitions-function #'consult-xref))

;;;; jump integration — connects with shell jump tool, M-g for quick directory jump
(defun amos/store-jump-history ()
  "Notify the jump tool that the current directory was visited to update scoring."
  (when (file-directory-p default-directory)
    (call-process "jump" nil 0 nil "chdir")))

;; Update jump score when entering a directory in dired
(add-hook 'dired-after-readin-hook #'amos/store-jump-history)

(defun amos/get-all-jump-dirs ()
  "Get all directories from the jump database (sorted by score), excluding root."
  (unless (file-directory-p default-directory) (cd "~"))
  (split-string (shell-command-to-string "jump top | sed \"s|^$HOME|~|\" | grep -x -v \"/\"") "\n" t))

(defun amos/consult-jumpdir ()
  "Fuzzy-select a directory from the jump database via consult and navigate to it."
  (interactive)
  (require 'consult)
  (let ((dir (consult--read
              (amos/get-all-jump-dirs)
              :prompt "Jump directory: "
              :require-match t
              :category 'file
              :state (consult--file-preview)
              :history 'file-name-history)))
    (when dir
      (find-file dir)
      (amos/store-jump-history))))

;; M-g evil normal binding — set directly after evil loads
(with-eval-after-load 'evil
  (define-key evil-normal-state-map (kbd "M-g") #'amos/consult-jumpdir))

;;;; Embark — context actions on candidates
;; C-.  opens an action menu on the candidate (e.g., open/copy/delete a file)
;; C-;  executes the default action directly
(use-package embark
  :bind (("C-."   . embark-act)
         ("C-;"   . embark-dwim)
         ("C-h B" . embark-bindings)
         :map minibuffer-local-map
         ("C-c C-e" . embark-export)
         ("C-c C-c" . embark-act)
         ("C-c C-o" . embark-export))
  :custom
  (prefix-help-command #'embark-prefix-help-command))

(use-package embark-consult
  :after (embark consult)
  :hook (embark-collect-mode . consult-preview-at-point-mode)
  :config
  ;; Export consult-grep/ripgrep results to a wgrep-editable buffer
  (setf (alist-get 'consult-location embark-exporters-alist)
        #'embark-consult-export-location-grep))

;;;; Corfu — in-buffer completion popup (replaces company-mode)
;; Auto-popup after 2 characters with 0.1s delay
(use-package corfu
  :hook (after-init . global-corfu-mode)
  :custom
  (corfu-cycle t)                       ; Cycle through candidates
  (corfu-auto t)                        ; Auto-popup
  (corfu-auto-prefix 2)                 ; Trigger after 2 characters
  (corfu-auto-delay 0.24)
  (corfu-preselect 'prompt)             ; Don't preselect first candidate
  (corfu-count 16)                      ; Show 16 candidates
  (corfu-max-width 120)
  (corfu-on-exact-match nil)            ; Don't auto-insert exact match
  (corfu-quit-at-boundary 'separator)   ; Quit at word boundary unless separator
  (corfu-quit-no-match 'separator)
  (corfu-popupinfo-delay '(0.5 . 0.2)) ; Documentation popup delay
  :config
  (corfu-popupinfo-mode 1)
  (corfu-history-mode 1)
  ;; Quit corfu when leaving insert state
  (add-hook 'evil-insert-state-exit-hook #'corfu-quit)
  ;; Save corfu history across sessions
  (with-eval-after-load 'savehist
    (add-to-list 'savehist-additional-variables 'corfu-history)))

;;;; Cape — completion-at-point extension backends
;; Add dabbrev (in-buffer word completion) and file path completion
(use-package cape
  :init
  (add-hook 'completion-at-point-functions #'cape-dabbrev)
  (add-hook 'completion-at-point-functions #'cape-file)
  ;; Make eglot capf composable with other backends
  (advice-add #'eglot-completion-at-point :around #'cape-wrap-nonexclusive))

;;;; Wgrep — editable grep result buffer
;; Edit grep/ripgrep results in-place; saving applies changes to files
(use-package wgrep
  :commands wgrep-change-to-wgrep-mode
  :custom
  (wgrep-auto-save-buffer t))

;;;; Custom consult commands (migrated from Doom +consult.el)

(defun amos/consult-ripgrep (&optional arg)
  "Ripgrep search in project. With C-u prefix, include .gitignore'd files."
  (interactive "P")
  (let ((consult-ripgrep-args
         (concat consult-ripgrep-args (if arg " --no-ignore" ""))))
    (consult-ripgrep)))

(defun amos/consult-ripgrep-cur-dir (&optional arg)
  "Ripgrep search in current directory. With C-u prefix, include ignored files."
  (interactive "P")
  (let ((consult-ripgrep-args
         (concat consult-ripgrep-args (if arg " --no-ignore" ""))))
    (consult-ripgrep default-directory)))

(defun amos/consult-find (&optional no-ignore)
  "Find file in project (fd). With C-u prefix, include ignored files."
  (interactive "P")
  (let ((consult-fd-args (append '("fd" "--color=never" "--hidden")
                                 (when no-ignore '("--no-ignore")))))
    (consult-fd nil nil)))

(defun amos/consult-find-cur-dir (&optional no-ignore)
  "Find file in current directory."
  (interactive "P")
  (let ((consult-fd-args (append '("fd" "--color=never" "--hidden")
                                 (when no-ignore '("--no-ignore")))))
    (consult-fd default-directory nil)))

(defun amos/consult-line ()
  "Search lines within the current buffer only."
  (interactive)
  (cl-letf (((symbol-function #'buffer-list)
             (lambda (&rest _) (list (current-buffer)))))
    (call-interactively #'consult-line-multi)))

;; ============================================================================
;; §5 Version Control
;; ============================================================================

;;;; Magit — the best Git interface
;; C-x g opens magit-status for staging, committing, pushing, rebasing, etc.
(use-package magit
  :bind (("C-x g"   . magit-status)
         ("C-c g b" . magit-blame))
  :custom
  (magit-diff-refine-hunk t)            ; Highlight word-level diff changes
  (magit-save-repository-buffers 'dontask)
  (magit-display-buffer-function
   #'magit-display-buffer-fullframe-status-topleft-v1)
  (magit-refresh-status-buffer nil)
  (magit-revision-insert-related-refs nil) ; Hide Parent/Merged/Contained headers
  :config
  (magit-auto-revert-mode 1)
  (add-hook 'git-commit-mode-hook (lambda () (setq-local fill-column 72)))
  (add-hook 'git-commit-setup-hook
            (lambda ()
              (when (and (bound-and-true-p evil-local-mode)
                         (not (evil-emacs-state-p))
                         (bobp) (eolp))
                (evil-insert-state))))
  ;; ESC in transient popups quits (like C-g)
  (with-eval-after-load 'transient
    (keymap-set transient-map "<escape>" #'transient-quit-one)
    (keymap-set transient-sticky-map "<escape>" #'transient-quit-one)))

;;;; git-gutter — show uncommitted change markers in the fringe
;; Shared helper: position indicator at right edge of realign-mode's left margin.
;; When multiple indicators appear on the same line, they are merged by
;; `amos/margin-merge-hook' (defined in realign-mode config below).
(defun amos/make-margin-indicator (indicator)
  "Pad INDICATOR to the right edge of realign-mode's left margin."
  (let* ((margin-w (or (car (window-margins)) 4))
         (ind-w (string-width indicator))
         (pad (max 0 (- margin-w ind-w 1))))
    (concat " " (make-string pad ? ) indicator)))

(use-package git-gutter
  :hook (after-init . global-git-gutter-mode)
  :config
  (advice-add #'git-gutter:set-window-margin :override #'ignore)
  ;; Custom change marker faces
  (defface amos/git-gutter-modified
    '((t (:foreground "chocolate" :weight bold :inherit default))) "Modified")
  (defface amos/git-gutter-added
    '((t (:foreground "ForestGreen" :weight bold :inherit default))) "Added")
  (defface amos/git-gutter-deleted
    '((t (:foreground "DarkRed" :weight bold :inherit default))) "Deleted")
  (defun amos/git-gutter-before-string (sign)
    (let* ((face (pcase sign
                   ("=" 'amos/git-gutter-modified)
                   ("+" 'amos/git-gutter-added)
                   ("-" 'amos/git-gutter-deleted)))
           (ovstring (propertize (amos/make-margin-indicator sign) 'face face)))
      (propertize " " 'display `((margin left-margin) ,ovstring))))
  (advice-add #'git-gutter:before-string :override #'amos/git-gutter-before-string)
  ;; Ensure realign margins are set before git-gutter renders signs
  (advice-add #'git-gutter:put-signs :before (lambda (&rest _) (realign-windows)))
  ;; Recenter after hunk navigation
  (advice-add #'git-gutter:next-hunk :after (lambda (_) (recenter)))
  (advice-add #'git-gutter:previous-hunk :after (lambda (_) (recenter)))
  ;; Refresh gutters after magit operations
  (with-eval-after-load 'magit
    (add-hook 'magit-post-refresh-hook #'git-gutter:update-all-windows)))

;;;; git-link — generate GitHub/GitLab URL for current code location
(use-package git-link :commands git-link)

;;;; git-timemachine — browse file's git history
(use-package git-timemachine :commands git-timemachine)

;;;; git-modes — syntax highlighting for .gitconfig/.gitignore/.gitattributes
(use-package git-modes
  :mode (("\\.gitconfig\\'"     . gitconfig-mode)
         ("\\.gitignore\\'"     . gitignore-mode)
         ("\\.gitattributes\\'" . gitattributes-mode)))

;; ============================================================================
;; §6 LSP & Diagnostics
;; ============================================================================

;;;; Eglot — built-in LSP client (Emacs 29+)
;; Lighter than lsp-mode, works out of the box, auto-discovers language servers
;; Install the corresponding LSP server for each language:
;;   Rust: rust-analyzer    Go: gopls         Python: pyright/pylsp
;;   C/C++: clangd          Java: jdtls       TypeScript: ts-ls
;;   OCaml: ocamllsp        YAML: yaml-ls     Zig: zls
(use-package eglot
  :ensure nil
  :hook ((rust-mode       . eglot-ensure)
         (rust-ts-mode    . eglot-ensure)
         (go-mode         . eglot-ensure)
         (go-ts-mode      . eglot-ensure)
         (python-mode     . eglot-ensure)
         (python-ts-mode  . eglot-ensure)
         (tuareg-mode     . eglot-ensure)
         (java-mode       . eglot-ensure)
         (java-ts-mode    . eglot-ensure)
         (yaml-mode       . eglot-ensure)
         (yaml-ts-mode    . eglot-ensure)
         (web-mode        . eglot-ensure)
         (js-mode         . eglot-ensure)
         (js-ts-mode      . eglot-ensure)
         (typescript-mode . eglot-ensure)
         (typescript-ts-mode . eglot-ensure)
         (c-mode          . eglot-ensure)
         (c++-mode        . eglot-ensure)
         (c-ts-mode       . eglot-ensure)
         (c++-ts-mode     . eglot-ensure)
         (zig-mode        . eglot-ensure))
  :custom
  (eglot-autoshutdown t)                ; Stop LSP when last buffer closes
  (eglot-sync-connect 0)                ; Non-blocking connection
  (eglot-extend-to-xref t)             ; Enable Eglot in xref'd files
  (eglot-report-progress nil)           ; Don't show progress in minibuffer
  (eglot-events-buffer-config '(:size 0 :format short)) ; Reduce logging
  :config
  ;; Disable symbol highlighting, inlay hints, and auto on-type formatting
  (setq eglot-ignored-server-capabilities
        '(:documentHighlightProvider :inlayHintProvider
          :documentOnTypeFormattingProvider
          :semanticTokensProvider))
  ;; Java JVM arguments
  (setq eglot-java-eclipse-jdt-args
        '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4"
          "-XX:AdaptiveSizePolicyWeight=90"
          "-Dsun.zip.disableMemoryMapping=true" "-Xmx4G" "-Xms1G"))
  ;; Pad code-action lightbulb to right edge of realign margin
  (defun amos/eglot-code-action-suggestion-a (orig-fn &rest args)
    "Advice around `eglot-code-action-suggestion' to pad margin indicator."
    (apply orig-fn args)
    (when (and (overlayp eglot--suggestion-overlay)
               (overlay-get eglot--suggestion-overlay 'before-string))
      (let* ((bs (overlay-get eglot--suggestion-overlay 'before-string))
             (disp (get-text-property 0 'display bs)))
        (when (and (consp disp) (consp (car disp)) (eq (caar disp) 'margin))
          (overlay-put eglot--suggestion-overlay 'before-string
                       (propertize " " 'display
                                   `((margin ,(cadar disp))
                                     ,(amos/make-margin-indicator (cadr disp)))))))))
  (advice-add #'eglot-code-action-suggestion :around #'amos/eglot-code-action-suggestion-a))

;;;; Flymake — built-in syntax checking framework
;; M-, / M-. jump to prev/next error (evil normal mode)
(use-package flymake
  :ensure nil
  :hook (prog-mode . flymake-mode)
  :custom
  (flymake-wrap-around nil)
  (flymake-show-diagnostics-at-end-of-line nil)
  ;; Don't let flymake manage margins — realign-mode handles them
  (flymake-autoresize-margins nil)
  :config
  ;; Prevent flymake from resetting window margins (would destroy realign-mode's centering)
  (advice-add #'flymake--apply-margins :override #'ignore)
  ;; Override margin indicators: single ● for all severity levels.
  ;; Must use `put' directly — flymake bakes type properties at load time,
  ;; so setting `flymake-margin-indicators-string' after load has no effect.
  (put 'flymake-error   'flymake-margin-string '("●" compilation-error))
  (put 'flymake-warning 'flymake-margin-string '("●" compilation-warning))
  (put 'flymake-note    'flymake-margin-string '("●" compilation-info))
  ;; Rewrite flymake overlay before-strings to use realign-mode margin padding.
  ;; Shows exactly one ● per line (highest severity wins).
  ;; Padding spaces use default face; only ● itself gets the diagnostic color.
  (defun amos/flymake--margin-before-string (type)
    "Build a margin before-string for flymake category TYPE."
    (when-let* ((margin-ind (get type 'flymake-margin-string))
                (ind-str (car margin-ind))
                (face (cadr margin-ind)))
      (let* ((margin-w (or (car (window-margins)) 4))
             (ind-w (string-width ind-str))
             (pad (max 0 (- margin-w ind-w 1)))
             (padded (concat (propertize (concat " " (make-string pad ?\s))
                                         'face 'default)
                             (propertize ind-str 'face `(:inherit ,face :underline nil)))))
        (propertize " " 'display `((margin left-margin) ,padded)))))

  (defun amos/flymake--reconcile-line (ov)
    "Ensure exactly one ● on the line containing overlay OV.
The highest-severity flymake overlay wins; all others get before-string nil."
    (let* ((bol (overlay-start ov))
           (eol (save-excursion (goto-char bol) (1+ (line-end-position))))
           (all-ovs (cl-remove-if-not
                     (lambda (o) (overlay-get o 'category))
                     (overlays-in bol eol)))
           ;; Keep only flymake overlays (category is flymake-error/warning/note)
           (fm-ovs (cl-remove-if-not
                    (lambda (o)
                      (get (overlay-get o 'category) 'flymake-margin-string))
                    all-ovs))
           ;; Sort by severity descending — highest first
           (sorted (sort fm-ovs
                         (lambda (a b)
                           (> (flymake--severity (overlay-get a 'category))
                              (flymake--severity (overlay-get b 'category)))))))
      (when sorted
        ;; Winner gets the ●, rest get nil
        (let ((winner (car sorted)))
          (overlay-put winner 'before-string
                       (amos/flymake--margin-before-string
                        (overlay-get winner 'category)))
          (dolist (loser (cdr sorted))
            (overlay-put loser 'before-string nil))))))

  (defun amos/flymake--highlight-line-a (orig-fn &rest args)
    "After flymake creates a diagnostic overlay, fix its margin before-string.
Ensures one ● per line with proper padding and face."
    ;; Ensure realign-mode margins are set so (window-margins) returns
    ;; the correct width — same trick as git-gutter:put-signs advice.
    (realign-windows)
    (let ((result (apply orig-fn args)))
      (when (overlayp result)
        (amos/flymake--reconcile-line result))
      result))
  (advice-add #'flymake--highlight-line :around #'amos/flymake--highlight-line-a)
  ;; Temporarily allow cursor past EOL when jumping to flymake errors, then restore
  (defun amos/flymake--restore-eol ()
    (setq evil-move-beyond-eol nil)
    (advice-remove #'evil-normal-post-command #'amos/flymake--restore-eol))
  (defun amos/flymake-goto-error (&optional prev)
    (setq evil-move-beyond-eol t)
    (advice-add #'evil-normal-post-command :after #'amos/flymake--restore-eol)
    (if prev (flymake-goto-prev-error 1)
      (flymake-goto-next-error 1)))
  (defun amos/flymake-goto-next-error ()
    (interactive) (amos/flymake-goto-error))
  (defun amos/flymake-goto-prev-error ()
    (interactive) (amos/flymake-goto-error t))
  (with-eval-after-load 'evil
    (define-key evil-normal-state-map (kbd "M-.") #'amos/flymake-goto-next-error)
    (define-key evil-normal-state-map (kbd "M-,") #'amos/flymake-goto-prev-error)))

;;;; Flymake diagnostic popup — child frame (like corfu, works in TUI)
;; Shows diagnostics on cursor hover using native tty-child-frames (Emacs 31+).
;; Only display in evil normal state.  Checks entire line for EOL errors.
(defvar amos/flymake-popup--frame nil "Child frame for flymake diagnostics.")
(defvar amos/flymake-popup--timer nil "Idle timer for flymake popup.")
(defvar amos/flymake-popup-delay 0.2 "Seconds of idle before showing popup.")
(defvar amos/flymake-popup-width 65 "Max width of diagnostic popup.")

(defvar amos/flymake-popup--frame-parameters
  '((no-accept-focus . t)
    (no-focus-on-map . t)
    (min-width . t)
    (min-height . t)
    (border-width . 0)
    (outer-border-width . 0)
    (internal-border-width . 0)
    (child-frame-border-width . 0)
    (vertical-scroll-bars . nil)
    (horizontal-scroll-bars . nil)
    (menu-bar-lines . 0)
    (tool-bar-lines . 0)
    (tab-bar-lines . 0)
    (tab-bar-lines-keep-state . t)
    (no-other-frame . t)
    (unsplittable . t)
    (undecorated . t)
    (fullscreen . nil)
    (cursor-type . nil)
    (no-special-glyphs . t)
    (desktop-dont-save . t))
  "Child frame parameters for flymake popup.")

(defface amos/flymake-popup
  '((t :inherit default))
  "Default face for flymake diagnostic popup.")

(defface amos/flymake-popup-border
  '((t :foreground "gray50" :inherit default))
  "Face for box-drawing border characters.")

(defun amos/flymake-popup--format-diag (diag)
  "Format DIAG to a single-line string with severity face."
  (let* ((text (car (split-string (flymake-diagnostic-text diag) "[\n\r]")))
         (face (flymake--lookup-type-property
                (flymake-diagnostic-type diag) 'mode-line-face)))
    (concat "● " (propertize text 'face face))))

(defun amos/flymake-popup--format-message (message)
  "Word-wrap MESSAGE to fit `amos/flymake-popup-width'."
  (let* ((lines (split-string message "\n"))
         (width (min amos/flymake-popup-width
                     (apply #'max (mapcar #'string-width lines)))))
    (mapcar (lambda (line)
              (with-temp-buffer
                (insert line)
                (move-to-column width t)
                (buffer-substring (point-min) (point))))
            lines)))

(defun amos/flymake-popup--ensure-frame ()
  "Create or return the child frame for flymake diagnostics."
  (let ((parent (window-frame)))
    (unless (and (frame-live-p amos/flymake-popup--frame)
                 (eq (frame-parent amos/flymake-popup--frame) parent))
      (when amos/flymake-popup--frame
        (delete-frame amos/flymake-popup--frame))
      (let ((buf (get-buffer-create " *flymake-diag*")))
        (with-current-buffer buf
          (setq-local mode-line-format nil
                      header-line-format nil
                      tab-line-format nil
                      frame-title-format ""
                      truncate-lines t
                      cursor-in-non-selected-windows nil
                      cursor-type nil
                      show-trailing-whitespace nil
                      display-line-numbers nil
                      left-fringe-width 0
                      right-fringe-width 0
                      left-margin-width 0
                      right-margin-width 0
                      fringes-outside-margins 0
                      buffer-read-only t)
          (face-remap-add-relative 'default 'amos/flymake-popup))
        (setq amos/flymake-popup--frame
              (make-frame
               `((name . "EmacsFlymakeTTY")
                 (parent-frame . ,parent)
                 (minibuffer . ,(minibuffer-window parent))
                 (width . 0) (height . 0) (visibility . nil)
                 (background-color
                  . ,(face-attribute 'amos/flymake-popup :background nil 'default))
                 ,@amos/flymake-popup--frame-parameters)))
        ;; Bind child frame window to our buffer (prevents inheriting current buffer)
        (let ((win (frame-root-window amos/flymake-popup--frame)))
          (set-window-buffer win buf)
          (set-window-dedicated-p win t)
          (set-window-parameter win 'no-delete-other-windows t)
          (set-window-parameter win 'no-other-window t))
        (redirect-frame-focus amos/flymake-popup--frame parent)))
    amos/flymake-popup--frame))

(defun amos/flymake-popup--show ()
  "Show flymake diagnostics for the current line in evil normal state."
  (amos/flymake-popup--hide)
  (when (and (bound-and-true-p evil-local-mode)
             (eq evil-state 'normal)
             (bound-and-true-p flymake-mode))
    (when-let* ((diags (or (flymake-diagnostics (point))
                           (flymake-diagnostics (line-beginning-position)
                                                (line-end-position)))))
      ;; Bind window-min-height/inhibit-redisplay around the entire
      ;; frame-create + size + position flow (same pattern as Corfu)
      ;; to prevent TTY child frame from reserving an extra mode-line row.
      (let* ((window-min-height 1)
             (window-min-width 1)
             (inhibit-redisplay t)
             (message (mapconcat #'amos/flymake-popup--format-diag diags "\n"))
             (lines (amos/flymake-popup--format-message message))
             (nlines (length lines))
             (content-w (apply #'max (mapcar #'string-width lines)))
             ;; Build box-drawing border around content
             (border-face 'amos/flymake-popup-border)
             (top (propertize (concat "┌" (make-string content-w ?─) "┐")
                              'face border-face))
             (bot (propertize (concat "└" (make-string content-w ?─) "┘")
                              'face border-face))
             (bordered-lines
              (mapcar (lambda (line)
                        (let ((pad (- content-w (string-width line))))
                          (concat (propertize "│" 'face border-face)
                                  line (make-string pad ?\s)
                                  (propertize "│" 'face border-face))))
                      lines))
             (full-text (concat top "\n"
                                (mapconcat #'identity bordered-lines "\n")
                                "\n" bot))
             ;; Frame dimensions in characters: content + 2 for borders
             (frame-w (+ content-w 2))
             (frame-h (+ nlines 2))
             (frame (amos/flymake-popup--ensure-frame))
             (buf (window-buffer (frame-root-window frame)))
             ;; Position in pixels
             (pos (posn-x-y (posn-at-point (point))))
             (ch (default-line-height))
             (cw (default-font-width))
             (edge (window-inside-pixel-edges))
             (pw (* frame-w cw))
             (ph (* frame-h ch))
             ;; X: align with cursor, clamped to frame width
             (x (max 0 (min (+ (car edge) (or (car pos) 0))
                            (- (frame-pixel-width) pw))))
             ;; Y: prefer below cursor, flip above if no space
             (yb (+ (cadr edge) (or (cdr pos) 0) ch))
             (y (if (> (+ yb ph) (frame-pixel-height))
                    (- yb ph ch)  ; above
                  yb)))               ; below
        (with-current-buffer buf
          (let ((inhibit-read-only t))
            (delete-region (point-min) (point-max))
            (insert full-text)
            (goto-char (point-min))))
        (set-frame-size frame (* frame-w cw) (* frame-h ch) t)
        (set-frame-position frame x y)
        (make-frame-visible frame)))))

(defun amos/flymake-popup--hide ()
  "Hide the flymake diagnostic child frame."
  (when (and (frame-live-p amos/flymake-popup--frame)
             (frame-visible-p amos/flymake-popup--frame))
    (make-frame-invisible amos/flymake-popup--frame)
    (with-current-buffer (window-buffer (frame-root-window amos/flymake-popup--frame))
      (let ((inhibit-read-only t))
        (delete-region (point-min) (point-max))))))

(defun amos/flymake-popup--post-command ()
  "Schedule flymake popup display after idle."
  (when amos/flymake-popup--timer
    (cancel-timer amos/flymake-popup--timer))
  (setq amos/flymake-popup--timer
        (run-with-timer amos/flymake-popup-delay nil
                        #'amos/flymake-popup--show)))

(defun amos/flymake-popup--update (&rest _)
  "Update popup if currently shown."
  (when (and (frame-live-p amos/flymake-popup--frame)
             (frame-visible-p amos/flymake-popup--frame))
    (amos/flymake-popup--show)))

(define-minor-mode amos/flymake-popup-mode
  "Show flymake diagnostics in a child frame on hover."
  :lighter nil
  (if amos/flymake-popup-mode
      (progn
        (add-hook 'pre-command-hook #'amos/flymake-popup--hide nil t)
        (add-hook 'post-command-hook #'amos/flymake-popup--post-command nil t)
        (advice-add #'flymake--handle-report :after #'amos/flymake-popup--update))
    (remove-hook 'pre-command-hook #'amos/flymake-popup--hide t)
    (remove-hook 'post-command-hook #'amos/flymake-popup--post-command t)
    (advice-remove #'flymake--handle-report #'amos/flymake-popup--update)
    (amos/flymake-popup--hide)))

;; Enable alongside flymake
(add-hook 'flymake-mode-hook
          (lambda () (amos/flymake-popup-mode (if flymake-mode 1 -1))))

;;;; Eldoc — show function signatures/docs in minibuffer (single-line to reduce noise)
(setq eldoc-echo-area-use-multiline-p nil)

;;;; breadcrumb — show code path in headerline (file > class > function)
(use-package breadcrumb
  :hook (eglot-managed-mode . breadcrumb-local-mode))

(defun amos/yank-flymake-error ()
  "Copy the Flymake diagnostic at point to kill-ring."
  (interactive)
  (when-let* ((diagnostic
               (mapconcat #'flymake-diagnostic-text
                          (flymake-diagnostics (point)) "\n")))
    (kill-new diagnostic)
    (message "Copied: %s" diagnostic)))

;; ============================================================================
;; §7 Language Support
;; ============================================================================

;;;; treesit-auto — automatically use tree-sitter major modes
;; Tree-sitter provides faster, more accurate syntax highlighting and structural editing
;; Grammar .so files are installed by system package manager; Emacs finds them via dynamic linker
(use-package treesit-auto
  :demand t
  :custom
  (treesit-auto-install nil)             ; Don't auto-download; use system-installed grammars
  (treesit-font-lock-level 4)            ; Maximum highlighting detail
  :config
  (treesit-auto-add-to-auto-mode-alist 'all)
  (global-treesit-auto-mode))

;; Language major modes — just install the package; LSP is managed by Eglot in §6
(use-package rust-mode       :mode "\\.rs\\'")
(use-package go-mode         :mode "\\.go\\'")
(use-package lua-mode        :mode "\\.lua\\'")
(use-package yaml-mode       :mode "\\.ya?ml\\'")
(use-package zig-mode        :mode "\\.zig\\'")
(use-package json-mode       :mode "\\.json\\'")
(use-package protobuf-mode   :mode "\\.proto\\'")
(use-package adoc-mode       :mode "\\.adoc\\'")
(use-package markdown-mode
  :mode ("\\.md\\'" "\\.markdown\\'")
  :custom (markdown-command "pandoc"))
(use-package tuareg                     ; OCaml
  :mode ("\\.ml\\'" "\\.mli\\'" "\\.mll\\'" "\\.mly\\'"))
(use-package web-mode
  :mode ("\\.html?\\'" "\\.css\\'" "\\.jsx?\\'" "\\.tsx?\\'")
  :custom
  (web-mode-markup-indent-offset 2)
  (web-mode-css-indent-offset 2)
  (web-mode-code-indent-offset 2))
(use-package typescript-mode :mode "\\.tsx?\\'")

;;;; C/C++ — cc-mode configuration (no LSP, use tree-sitter for highlighting)
(use-package cc-mode
  :ensure nil
  :mode ("\\.mm\\'" . objc-mode)
  :init
  (setq-default c-basic-offset 4
                c-backspace-function #'delete-backward-char
                c-default-style "amos")
  (defalias 'cpp-mode 'c++-mode)
  :config
  ;; Custom C style matching Doom config
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
                                     (member-init-intro . +)))))
  ;; .h files → c++-mode by default
  (add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))
  ;; Treat constexpr as noise macro (better indentation)
  (setq-default c-noise-macro-names '("constexpr")))

;; c++-ts-mode indentation (treesit-based, used when treesit-auto activates c++-ts-mode)
(with-eval-after-load 'c-ts-mode
  (setq c-ts-mode-indent-offset 4
        c-ts-mode-indent-style 'gnu)

  ;; top-level-namespace: don't indent inside top-level namespace
  (cl-pushnew
   (cons 'top-level-namespace
         (lambda ()
           (lambda (node parent &rest _)
             (when-let* ((gp (treesit-node-parent parent))
                         (ggp (treesit-node-parent gp)))
               (and
                (string-match-p "namespace_definition" (treesit-node-type gp))
                (string-match-p "translation_unit" (treesit-node-type ggp)))))))
   treesit-simple-indent-presets)

  ;; Override c-ts-mode indent rules with custom style
  (defun amos/c-ts-mode--simple-indent-rules (mode style)
    "Custom indent rules for C/C++ ts-mode."
    (let ((rules
           `(;; tree-sitter specific rules
             (c-ts-mode--for-each-tail-body-matcher
              prev-line c-ts-mode-indent-offset)

             ;; Top-level namespace: no indent
             ((top-level-namespace) parent-bol 0)

             ;; Compound statement after control flow: align with parent
             ((query "(for_statement body: (compound_statement \"{\") @indent)") parent-bol 0)
             ((query "(if_statement consequence: (compound_statement \"{\") @indent)") parent-bol 0)
             ((query "(else_clause (compound_statement \"{\") @indent)") parent-bol 0)
             ((query "(else_clause (_) @indent)") parent-bol 4)
             ((parent-is "else_statement") parent-bol 4)
             ((query "(while_statement body: (compound_statement \"{\") @indent)") parent-bol 0)
             ((query "(switch_statement body: (_) @indent)") parent-bol 0)
             ((query "(case_statement (compound_statement _) @indent)") parent-bol 0)
             ((query "(do_statement body: (_) @indent)") parent-bol 0)
             ((parent-is "switch_statement") standalone-parent c-ts-mode-indent-offset)

             ;; Misc
             ((parent-is "translation_unit") column-0 0)
             ((node-is ,(rx (or "else" "case"))) standalone-parent 0)
             ((match "while" "do_statement") parent 0)
             c-ts-mode--parenthesized-expression-indent-rule
             c-ts-mode--for-loop-indent-rule
             c-ts-mode--label-indent-rules
             ,@c-ts-mode--preproc-indent-rules
             c-ts-mode--macro-heuristic-rules

             ;; Type/function definitions
             ((parent-is ,(rx (or "function_definition"
                                  "struct_specifier"
                                  "enum_specifier"
                                  "function_declarator"
                                  "template_declaration")))
              parent 0)
             ((match "function_declarator" nil "declarator") parent-bol 0)

             ;; Comments
             ((and (parent-is "comment") c-ts-common-looking-at-star)
              c-ts-common-comment-start-after-first-star -1)
             (c-ts-common-comment-2nd-line-matcher
              c-ts-common-comment-2nd-line-anchor 1)
             ((parent-is "comment") prev-adaptive-prefix 0)

             ;; Preproc
             ((node-is "preproc_arg") no-indent)
             ((node-is "preproc") column-0 0)
             ((node-is "#endif") column-0 0)

             ;; C++ access specifiers
             ((node-is "access_specifier") parent-bol 0)
             ((prev-line-is "access_specifier")
              parent-bol c-ts-mode-indent-offset)

             c-ts-common-baseline-indent-rule)))
      (pcase mode
        ('c `((c . ,rules)))
        ('cpp `((cpp .
                 (((query "(for_range_loop (compound_statement \"{\") @indent)") parent-bol 0)
                  ,@rules)))))))

  (advice-add #'c-ts-mode--simple-indent-rules :override #'amos/c-ts-mode--simple-indent-rules)

  ;; Electric indent + fill-column for ts modes
  (add-hook 'c-ts-base-mode-hook
            (lambda ()
              (electric-indent-local-mode 1)
              (setq-local fill-column 120))))

;; modern-cpp-font-lock — better C++ syntax highlighting (C++11/14/17/20 keywords)
(use-package modern-cpp-font-lock
  :hook (c++-mode . modern-c++-font-lock-mode))
;;;; restclient — HTTP client for testing REST APIs
(use-package restclient
  :mode ("\\.http\\'" . restclient-mode)
  :config
  (setq restclient-method-body-prohibited-regexp "^HEAD$")

  ;; Prevent response buffer from stealing focus
  (advice-add 'switch-to-buffer-other-window :around
              (lambda (orig-fn buffer &rest args)
                (if (and (bufferp buffer)
                         (string-match-p "\\*HTTP Response\\*" (buffer-name buffer)))
                    (display-buffer buffer)
                  (apply orig-fn buffer args))))

  ;; Prepend default host to relative URLs (e.g. /index → http://127.0.0.1:9200/index)
  (defun amos/restclient-prepend-url-once (url &rest _)
    "Prepend 127.0.0.1:9200 to URL if it has no host. Remove self after first run."
    (let ((new-url (if (string-match-p "^http" url)
                       url
                     (concat "http://127.0.0.1:9200" url))))
      (advice-remove #'restclient-replace-all-in-string #'amos/restclient-prepend-url-once)
      new-url))

  (advice-add 'restclient-http-parse-current-and-do
              :before
              (lambda (&rest _)
                (advice-add 'restclient-replace-all-in-string
                            :filter-return #'amos/restclient-prepend-url-once))))

;; Additional settings for built-in modes
(setq python-indent-guess-indent-offset-verbose nil)
(setq sh-indent-after-continuation 'always)

;; ============================================================================
;; §8 Org mode
;; ============================================================================

(use-package org
  :ensure nil
  :hook (org-mode . visual-line-mode)   ; Auto-wrap in org
  :custom
  (org-startup-folded nil)              ; Don't fold on open
  (org-use-fast-todo-selection nil)
  (org-M-RET-may-split-line '((default)))
  (org-image-actual-width '(400))       ; Limit image display width
  (org-src-tab-acts-natively t)         ; TAB indents per language in src blocks
  (org-goto-interface 'outline-path-completion)
  (org-outline-path-complete-in-steps nil)

  ;; Agenda — GTD todo list
  (org-agenda-files '("~/org/todo.org"))

  ;; Capture — quick idea capture (triggered by C-c c)
  (org-capture-templates
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

  ;; Babel — execute code blocks within org
  (org-babel-load-languages
   '((python . t) (emacs-lisp . t) (dot . t) (C . t) (sql . t) (awk . t)))

  ;; LaTeX export — XeLaTeX + Beamer
  (org-latex-compiler "xelatex")
  (org-latex-tables-booktabs t)
  (org-preview-latex-default-process 'imagemagick)
  (org-beamer-frame-level 2)
  (org-beamer-theme "metropolis")
  (org-latex-text-markup-alist
   '((bold . "\\textbf{%s}") (code . protectedtexttt)
     (italic . "\\emph{%s}") (strike-through . "\\emph{%s}")
     (underline . "\\uline{%s}") (verbatim . protectedtexttt)))

  ;; HTML export
  (org-html-text-markup-alist
   '((bold . "<b>%s</b>") (code . "<code>%s</code>")
     (italic . "<i>%s</i>")
     (strike-through . "<strong style=\"color : red;\">%s</strong>")
     (underline . "<span class=\"underline\">%s</span>")
     (verbatim . "<code>%s</code>")))

  :config
  (org-babel-do-load-languages
   'org-babel-load-languages org-babel-load-languages))

;;;; citar — modern bibliography management (replaces org-ref + ivy-bibtex)
(use-package citar
  :custom
  (citar-bibliography '("~/git/serverconfig/amosbird.bib"))
  (org-cite-global-bibliography '("~/git/serverconfig/amosbird.bib"))
  (org-cite-insert-processor 'citar)
  (org-cite-follow-processor 'citar)
  (org-cite-activate-processor 'citar))

;;;; ox-hugo — export org to Hugo blog
(use-package ox-hugo
  :after org
  :custom (org-hugo-default-section-directory "post"))

;; ============================================================================
;; §9 Tools
;; ============================================================================

;;;; envrc — auto-load project .envrc files (replaces direnv.el)
(use-package envrc
  :hook (after-init . envrc-global-mode)
  :bind (("C-x d" . envrc-allow)
         ("C-x a" . envrc-reload)))

;;;; yasnippet — code snippets/templates
(use-package yasnippet
  :hook (prog-mode . yas-minor-mode)
  :custom
  (yas-triggers-in-field nil)
  (yas-snippet-dirs (list (expand-file-name "snippets" user-emacs-directory)))
  :config
  ;; C-l in snippet fields — jump to next field
  (define-key yas-keymap (kbd "C-l") #'yas-next-field)
  (yas-reload-all))

;;;; deadgrep — interactive ripgrep frontend (better than consult-ripgrep for large searches)
(use-package deadgrep :commands deadgrep)

;;;; apheleia — async code formatter (clang-format, black, prettier, etc.)
;; C-S-f / S-F11 to format region or buffer on demand. No format-on-save.
(use-package apheleia
  :commands (apheleia-format-buffer)
  :init
  (defun amos/format-region-or-buffer ()
    "Format the selected region, or whole buffer if nothing is selected."
    (interactive)
    (require 'apheleia)
    (if (use-region-p)
        ;; For region: try eglot range formatting if available, else apheleia
        (if (and (bound-and-true-p eglot--managed-mode)
                 (eglot-server-capable :documentRangeFormattingProvider))
            (eglot-format (region-beginning) (region-end))
          (call-interactively #'apheleia-format-buffer))
      (call-interactively #'apheleia-format-buffer)))
  (with-eval-after-load 'evil
    (dolist (map (list evil-normal-state-map evil-visual-state-map))
      (define-key map (kbd "C-S-f") #'amos/format-region-or-buffer)
      (define-key map (kbd "S-<f11>") #'amos/format-region-or-buffer)))
  :config
  ;; Use LSP formatter (eglot) when available, chain with tool formatters
  (add-to-list 'apheleia-formatters '(lsp . (lambda (&rest args)
                                               (when (bound-and-true-p eglot--managed-mode)
                                                 (eglot-format)))))
  ;; C/C++ use clang-format
  (setf (alist-get 'c-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c-ts-mode apheleia-mode-alist) 'clang-format)
  (setf (alist-get 'c++-ts-mode apheleia-mode-alist) 'clang-format))

;;;; C/C++ helpers — used by snippets and cc-playground

(defun amos/add-include (h &rest others)
  "Add #include <H> near top of file, avoiding duplicates."
  (interactive "M#include: ")
  (dolist (header (cons h others))
    (let ((incl (format "#include <%s>" header)))
      (save-excursion
        (unless (save-excursion (goto-char (point-min)) (search-forward incl nil t))
          (goto-char (point-min))
          (when (re-search-forward "\\(#include\\|#pragma once\\)" nil t)
            (when (looking-back "#pragma once" (line-beginning-position))
              (forward-line))
            (forward-line)
            (beginning-of-line))
          (insert incl "\n"))))))

;; Alias for Doom snippet compatibility
(defalias '+amos/add-include #'amos/add-include)

;;;; cc-playground — C/C++ scratch pad for quick experiments
(use-package cc-playground
  :ensure nil
  :load-path "lisp/"
  :commands (cc-playground cc-playground-mode cc-playground-find-snippet cc-playground-leetcode)
  :bind (:map cc-playground-mode-map
              ("<f8>" . cc-playground-rm)
              ("S-<return>" . cc-playground-rm)
              ("C-c r" . cc-playground-add-or-modify-tag)
              ("C-c b" . cc-playground-bench)
              ("C-c d" . cc-playground-debug)
              ("C-c t" . cc-playground-debug-test)
              ("C-c c" . cc-playground-change-compiler)
              ("C-c o" . cc-playground-switch-optimization-flag)
              ("C-c f" . cc-playground-add-compilation-flags))
  :init
  ;; Mark dir-local variables as safe
  (put 'cc-exec 'safe-local-variable #'stringp)
  (put 'cc-flags 'safe-local-variable #'stringp)
  (put 'cc-links 'safe-local-variable #'stringp)
  ;; Switch to normal state before exec/debug (so output is readable)
  (dolist (fn '(cc-playground-exec cc-playground-debug cc-playground-exec-test cc-playground-bench))
    (advice-add fn :before (lambda (&rest _) (evil-normal-state))))
  ;; Auto-enable cc-playground-mode for files under playground dir
  (defun amos/maybe-enable-cc-playground ()
    (when (and buffer-file-name
               (string-prefix-p (expand-file-name "~/cc-playground")
                                (file-truename buffer-file-name)))
      (cc-playground-mode 1)))
  (add-hook 'find-file-hook #'amos/maybe-enable-cc-playground)
  :config
  (setq cc-playground-basedir "~/cc-playground")
  (when (fboundp 'rmsbolt-mode)
    (add-hook 'cc-playground-mode-hook (lambda () (rmsbolt-mode 1)))))

;;;; Tmux / Kitty integration — Emacs ↔ terminal interop
(defun amos/tmux-detach ()
  "Detach tmux client."
  (interactive)
  (call-process "tmux" nil 0 nil "detach-client"))

(defun amos/tmux-fork-window (&optional command prompt)
  "Open a new tmux window and cd to the current directory."
  (interactive)
  (amos/store-jump-history)
  (if command
      (call-process-shell-command
       (format "tmux switch-client -t amos; tmuxkillwindow amos:%s; tmux run -t amos \"tmux new-window -n %s -c %s; tmux send-keys %s C-m\""
               (or prompt "nil") (or prompt "nil") default-directory command)
       nil 0)
    (call-process-shell-command
     (format "tmux switch-client -t amos; tmux run -t amos \"tmux new-window -c %s\"" default-directory)
     nil 0)))

(defun amos/tmux-split-window ()
  "Split a tmux pane and cd to the current directory."
  (interactive)
  (amos/store-jump-history)
  (call-process-shell-command
   (format "tmux split-window -c %s" default-directory) nil 0))

(defun amos/tmux-source ()
  "Reload tmux configuration."
  (interactive)
  (call-process-shell-command "tmux source-file ~/.tmux/.tmux.conf.emacs" nil 0))

(defun amos/kitty-fork-window (&optional command prompt)
  "Open a new window in kitty/tmux."
  (interactive)
  (amos/store-jump-history)
  (if command
      (call-process-shell-command
       (format "kitten @ --to unix:/tmp/kitty_sock action next_window; export TMUX=$TMPDIR/tmux-amos; tmux switch-client -t amos; tmuxkillwindow amos:%s; tmux run -t amos \"tmux new-window -n %s -c %s; tmux send-keys %s C-m\""
               (or prompt "nil") (or prompt "nil") default-directory command)
       nil 0)
    (call-process-shell-command
     (format "kitten @ --to unix:/tmp/kitty_sock action next_window; export TMUX=$TMPDIR/tmux-amos; tmux switch-client -t amos; tmux run -t amos \"tmux new-window -c %s\"" default-directory)
     nil 0)))

(defun amos/launch ()
  "Run launch.sh in the project root directory."
  (interactive)
  (when (buffer-file-name)
    (save-buffer))
  (let ((default-directory (or (vc-root-dir) default-directory)))
    (amos/kitty-fork-window " launch.sh" (getenv "envprompt"))))

(defun amos/run-script ()
  "Compile/run the current script."
  (interactive)
  (evil-normal-state)
  (compile (buffer-file-name) t))

;; C-RET runs script in script modes
(dolist (hook-map '((sh-base-mode-hook   . sh-base-mode-map)
                    (python-base-mode-hook . python-base-mode-map)))
  (add-hook (car hook-map)
            (lambda () (local-set-key (kbd "<C-return>") #'amos/run-script))))

;;;; Shell command helpers
(defun amos/shell-command-or-region ()
  "Run shell command on region if active, otherwise execute a shell command."
  (interactive)
  (if (use-region-p)
      (shell-command-on-region (region-beginning) (region-end)
                               (read-shell-command "Shell command on region: ") nil t)
    (call-interactively #'shell-command)))

(defun amos/shell-command-on-buffer (command)
  "Run shell command on the entire buffer, replacing its contents."
  (interactive "sShell command on buffer: ")
  (shell-command-on-region (point-min) (point-max) command nil t))

(defun amos/revert-all-buffers ()
  "Revert all file-visiting buffers."
  (interactive)
  (dolist (buf (buffer-list))
    (let ((filename (buffer-file-name buf)))
      (when (and filename (not (buffer-modified-p buf)))
        (if (file-readable-p filename)
            (with-current-buffer buf
              (revert-buffer :ignore-auto :noconfirm :preserve-modes))
          (kill-buffer buf)))))
  (message "All file buffers reverted."))

;;;; speed-type — typing practice
(use-package speed-type :commands speed-type-text)

;;;; Ediff — built-in diff tool, side-by-side without a new frame
(setq ediff-window-setup-function 'ediff-setup-windows-plain
      ediff-split-window-function 'split-window-horizontally)

;; Ediff keybindings — j/k navigate, c merges both A+B into C
(with-eval-after-load 'ediff
  (defun amos/ediff-copy-both-to-C ()
    "Copy both A and B regions concatenated into the merge buffer C."
    (interactive)
    (amos/with-private-kill-ring
     (ediff-copy-diff ediff-current-difference nil 'C nil
                      (concat
                       (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                       (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer)))))

  ;; Make ediff-copy-A-to-C and ediff-copy-B-to-C use private kill ring
  (advice-add #'ediff-copy-A-to-C :around
              (lambda (orig-fn &rest args)
                (amos/with-private-kill-ring (apply orig-fn args))))
  (advice-add #'ediff-copy-B-to-C :around
              (lambda (orig-fn &rest args)
                (amos/with-private-kill-ring (apply orig-fn args))))
  (advice-add #'ediff-restore-diff-in-merge-buffer :around
              (lambda (orig-fn &rest args)
                (amos/with-private-kill-ring (apply orig-fn args))))

  (add-hook 'ediff-keymap-setup-hook
            (lambda ()
              (define-key ediff-mode-map "c" #'amos/ediff-copy-both-to-C))))

;;;; realign-mode — auto-center window content (useful for widescreen)
;; Not on MELPA; placed in lisp/ directory (from github.com/amosbird/realign-mode.el)
(use-package realign-mode
  :ensure nil
  :load-path "lisp/"
  :hook (after-init . realign-mode)
  :config
  ;; Skip realign in child frames (corfu popup) and ediff
  (defun amos/realign-ignore-window-p (window)
    (let* ((buffer (window-buffer window))
           (buffname (string-trim (buffer-name buffer))))
      (or (frame-parent (window-frame window))
          (equal buffname "*Ediff Control Panel*"))))
  (push #'amos/realign-ignore-window-p realign-ignore-window-predicates)

  ;; ── Margin indicator merge ──
  ;; When multiple packages (git-gutter, flymake, eglot) place margin indicators
  ;; on the same line, merge them into a single string (e.g. "💡!+").
  (defvar-local amos--margin-merge-line nil
    "Line-beginning position of the last merged line, or nil.")

  (defun amos--margin-extract-indicator (ov)
    "Extract raw indicator string from OV's margin before-string, or nil."
    (when-let* ((bs (overlay-get ov 'before-string))
                (disp (and (stringp bs) (get-text-property 0 'display bs))))
      (when (and (consp disp) (consp (car disp)) (eq (caar disp) 'margin))
        (let ((s (cadr disp)))
          (when (stringp s) (string-trim s))))))

  (defun amos--margin-restore (bol)
    "Restore saved before-strings on line at BOL."
    (let ((eol (save-excursion (goto-char bol) (1+ (line-end-position)))))
      (dolist (ov (overlays-in bol eol))
        (when-let ((saved (overlay-get ov 'amos--margin-saved)))
          (overlay-put ov 'before-string saved)
          (overlay-put ov 'amos--margin-saved nil)))))

  (defun amos--margin-merge-at (bol)
    "Merge margin indicators on line at BOL. Returns t if merged."
    (let ((eol (save-excursion (goto-char bol) (1+ (line-end-position))))
          entries)
      (dolist (ov (overlays-in bol eol))
        (when-let ((ind (amos--margin-extract-indicator ov)))
          (when (> (length ind) 0)
            (push (cons ov ind) entries))))
      (when (> (length entries) 1)
        (setq entries (nreverse entries))
        (let* ((merged (mapconcat #'cdr entries))
               (primary (caar entries)))
          (dolist (entry entries)
            (overlay-put (car entry) 'amos--margin-saved
                         (overlay-get (car entry) 'before-string))
            (unless (eq (car entry) primary)
              (overlay-put (car entry) 'before-string nil)))
          (overlay-put primary 'before-string
                       (propertize " " 'display
                                   `((margin left-margin)
                                     ,(amos/make-margin-indicator merged)))))
        t)))

  (defun amos/margin-merge-hook ()
    "Post-command hook: merge margin indicators at point, restore previous line."
    (let ((cur (line-beginning-position)))
      (when (and amos--margin-merge-line
                 (/= amos--margin-merge-line cur))
        (ignore-errors (amos--margin-restore amos--margin-merge-line))
        (setq amos--margin-merge-line nil))
      (when (amos--margin-merge-at cur)
        (setq amos--margin-merge-line cur))))

  (add-hook 'post-command-hook #'amos/margin-merge-hook))

;;;; Dired — built-in file manager
(use-package dired
  :ensure nil
  :custom
  (dired-dwim-target t)                ; Auto-infer target in dual-pane layout
  (dired-recursive-copies 'always)
  (dired-recursive-deletes 'top)       ; Recursive delete confirms only at top level
  (dired-create-destination-dirs 'always)
  (dired-deletion-confirmer 'y-or-n-p)
  (dired-clean-confirm-killing-deleted-buffers nil)
  (dired-auto-revert-buffer 'dired-buffer-stale-p)
  (dired-vc-rename-file t)
  (dired-free-space nil)
  (dired-listing-switches "-alh")
  (dired-omit-verbose nil)
  (dired-omit-files "\\`[.]\\'" )      ; Hide dotfiles
  :config
  (setq ls-lisp-dirs-first t)
  (setq ls-lisp-verbosity nil)

  ;; Evil bindings (aligned with Doom dired)
  (with-eval-after-load 'evil
    (evil-define-key* 'normal dired-mode-map
      "q"   #'quit-window
      "j"   #'dired-next-line
      "k"   #'dired-previous-line
      "h"   #'dired-up-directory
      "l"   #'dired-find-file
      (kbd "RET") #'dired-find-file
      "d"   #'dired-flag-file-deletion
      "D"   #'dired-do-delete
      "x"   #'dired-do-flagged-delete
      "f"   #'find-file
      "F"   #'dired-do-copy
      "R"   #'dired-do-rename
      "z"   #'dired-do-compress
      "i"   #'dired-create-directory
      "m"   #'dired-mark
      "u"   #'dired-unmark
      "w"   #'dired-copy-filename-as-kill
      "W"   (lambda () (interactive) (dired-copy-filename-as-kill 0))
      "E"   #'wdired-change-to-wdired-mode
      "!"   #'dired-do-shell-command
      (kbd "C-f") #'dired-omit-mode
      (kbd "SPC") nil)))

(use-package dired-x :ensure nil
  :hook (dired-mode . dired-omit-mode))

;;;; dired-ranger — file copy/paste (ranger-style)
(use-package dired-ranger
  :after dired
  :config
  (with-eval-after-load 'evil
    (evil-define-key* 'normal dired-mode-map
      "c"   (lambda () (interactive) (dired-ranger-copy t))
      "y"   (lambda () (interactive) (dired-ranger-copy nil))
      "p"   #'dired-ranger-paste
      "P"   #'dired-ranger-move)))

(use-package dired-subtree              ; TAB expands subdirectories
  :after dired
  :bind (:map dired-mode-map ("TAB" . dired-subtree-toggle)))

(use-package dired-quick-sort           ; s for quick sorting
  :after dired
  :config (dired-quick-sort-setup))

;;;; dired-open — open files with external programs
(use-package dired-open
  :after dired
  :custom
  (dired-open-extensions
   '(("pdf" . "xdg-open")
     ("mp3" . "xdg-open") ("mp4" . "xdg-open") ("mkv" . "xdg-open")
     ("webm" . "xdg-open") ("flac" . "xdg-open") ("mov" . "xdg-open")
     ("jpg" . "xdg-open") ("jpeg" . "xdg-open") ("png" . "xdg-open")
     ("gif" . "xdg-open") ("svg" . "xdg-open")
     ("doc" . "xdg-open") ("docx" . "xdg-open")
     ("xls" . "xdg-open") ("xlsx" . "xdg-open")
     ("ppt" . "xdg-open") ("pptx" . "xdg-open"))))

;;;; Compilation
(setq compilation-ask-about-save nil
      compilation-always-kill t
      compilation-scroll-output 'first-error)

;;;; persistent-scratch — persist *scratch* content with git auto-backup
;;
;; Architecture:
;;   ~/.emacs.d/var/scratch-git/     ← dedicated git repo
;;   ├── scratch.txt                 ← *scratch* buffer content
;;   └── scratch-<project>.txt      ← project scratch (future)
;;
;; Flow:  buffer modified → idle 5s → write file
;;        file written → debounce 60s → magit-call-git add + commit
;;        kill-emacs → immediate save + commit
;;
;; Git operations use magit-call-git (silent, no buffer, no refresh).
;; Interactive commands (history, diff) use full magit UI.
;; Recovery: content auto-restores on startup; SPC I for full magit-log.

(defvar amos/scratch-dir
  (expand-file-name "scratch-git" minimal-emacs-var-dir)
  "Directory for the persistent scratch git repository.")

(defvar amos/scratch-file
  (expand-file-name "scratch.txt" amos/scratch-dir)
  "Path to the persistent scratch file.")

(defvar amos/scratch-idle-delay 5
  "Seconds of idle time before saving scratch buffer to file.")

(defvar amos/scratch-commit-delay 60
  "Seconds to debounce before committing scratch changes to git.")

(defvar amos/scratch--save-timer nil
  "Idle timer for saving scratch buffer to file.")

(defvar amos/scratch--commit-timer nil
  "Debounce timer for git commit.")

(defvar amos/scratch--last-content nil
  "Last saved content hash, to avoid redundant writes.")

(defun amos/scratch--git (&rest args)
  "Run git in scratch repo silently via magit-call-git.
Falls back to call-process if magit is not yet loaded."
  (let ((default-directory amos/scratch-dir))
    (if (fboundp 'magit-call-git)
        (apply #'magit-call-git args)
      (apply #'call-process "git" nil nil nil args))))

(defun amos/scratch--git-output (&rest args)
  "Run git in scratch repo, return stdout as string."
  (let ((default-directory amos/scratch-dir))
    (if (fboundp 'magit-git-string)
        (or (apply #'magit-git-string args) "")
      (with-output-to-string
        (with-current-buffer standard-output
          (apply #'call-process "git" nil t nil args))))))

(defun amos/scratch--ensure-git-repo ()
  "Initialize scratch directory as a git repo if needed."
  (make-directory amos/scratch-dir t)
  (unless (file-directory-p (expand-file-name ".git" amos/scratch-dir))
    (amos/scratch--git "init")
    (amos/scratch--git "commit" "--allow-empty" "-m" "init scratch repo")))

(defun amos/scratch--content-hash ()
  "Return hash of current *scratch* content, or nil if buffer doesn't exist."
  (when-let* ((buf (get-buffer "*scratch*")))
    (with-current-buffer buf
      (secure-hash 'sha1 (buffer-substring-no-properties (point-min) (point-max))))))

(defun amos/scratch-save ()
  "Save *scratch* buffer content to file (only if changed)."
  (when-let* ((buf (get-buffer "*scratch*")))
    (let ((hash (amos/scratch--content-hash)))
      (when (and hash (not (equal hash amos/scratch--last-content)))
        (with-current-buffer buf
          (let* ((content (buffer-substring-no-properties (point-min) (point-max)))
                 (mode (symbol-name major-mode))
                 (tmp-file (concat amos/scratch-file ".tmp")))
            ;; Atomic write: tmp file → rename
            (let ((coding-system-for-write 'utf-8-unix))
              (with-temp-file tmp-file
                (insert (format "-*- mode: %s -*-\n" mode))
                (insert content)))
            (rename-file tmp-file amos/scratch-file t)
            (setq amos/scratch--last-content hash)
            ;; Schedule git commit (debounced)
            (amos/scratch--schedule-commit)))))))

(defun amos/scratch--schedule-commit ()
  "Schedule a debounced git commit."
  (when amos/scratch--commit-timer
    (cancel-timer amos/scratch--commit-timer))
  (setq amos/scratch--commit-timer
        (run-at-time amos/scratch-commit-delay nil #'amos/scratch-commit)))

(defun amos/scratch-commit ()
  "Git add + commit the scratch file (silent, no magit refresh)."
  (setq amos/scratch--commit-timer nil)
  (when (file-exists-p amos/scratch-file)
    (let ((fname (file-name-nondirectory amos/scratch-file)))
      ;; Check if there are actual changes to commit
      (when (not (string-empty-p (amos/scratch--git-output "status" "--porcelain" fname)))
        (amos/scratch--git "add" fname)
        (amos/scratch--git "commit" "-m"
                           (format-time-string "auto-save %Y-%m-%d %H:%M:%S"))))))

(defun amos/scratch-restore ()
  "Restore *scratch* buffer content from the persistent file."
  (when (file-readable-p amos/scratch-file)
    (with-current-buffer (get-buffer-create "*scratch*")
      (let* ((coding-system-for-read 'utf-8-unix)
             (content (with-temp-buffer
                        (insert-file-contents amos/scratch-file)
                        (buffer-string)))
             ;; Parse mode from first line: -*- mode: foo -*-
             (mode (when (string-match "-\\*- mode: \\([^ ]+\\) -\\*-" content)
                     (intern (match-string 1 content))))
             ;; Strip the mode line from content
             (body (if (string-match "\n" content)
                       (substring content (1+ (match-beginning 0)))
                     "")))
        (erase-buffer)
        (insert body)
        (goto-char (point-min))
        (when (and mode (fboundp mode))
          (funcall mode))
        (set-buffer-modified-p nil)
        (setq amos/scratch--last-content (amos/scratch--content-hash))))))

(defun amos/scratch--kill-emacs-save ()
  "Final save + commit on Emacs exit."
  (when amos/scratch--commit-timer
    (cancel-timer amos/scratch--commit-timer)
    (setq amos/scratch--commit-timer nil))
  (amos/scratch-save)
  (amos/scratch-commit))

(defun amos/scratch-browse-history ()
  "Browse scratch git history in magit-log."
  (interactive)
  (require 'magit)
  (let ((default-directory amos/scratch-dir))
    (magit-log-other (list "HEAD") nil
                     (list "--follow" "--"
                           (file-name-nondirectory amos/scratch-file)))))

(defun amos/scratch-diff ()
  "Show uncommitted scratch changes in magit-diff."
  (interactive)
  (require 'magit)
  (let ((default-directory amos/scratch-dir))
    (magit-diff-unstaged)))

(defun amos/scratch-status ()
  "Open magit-status for the scratch repo."
  (interactive)
  (require 'magit)
  (magit-status amos/scratch-dir))

(defun amos/scratch-setup ()
  "Initialize the persistent scratch system."
  (amos/scratch--ensure-git-repo)
  (amos/scratch-restore)
  ;; Idle timer: save file when Emacs is idle
  (setq amos/scratch--save-timer
        (run-with-idle-timer amos/scratch-idle-delay t #'amos/scratch-save))
  (add-hook 'kill-emacs-hook #'amos/scratch--kill-emacs-save))

;; Bootstrap: run after init to avoid slowing startup
(add-hook 'emacs-startup-hook #'amos/scratch-setup)

;;;; Uniquify — disambiguate buffer names with directory path
(use-package uniquify
  :ensure nil
  :custom
  (uniquify-buffer-name-style 'forward)  ; show dir/filename instead of filename<2>
  (uniquify-after-kill-buffer-p t))

;;;; Auto-revert — keep buffers in sync with disk changes
(use-package autorevert
  :ensure nil
  :hook (after-init . global-auto-revert-mode)
  :custom
  (auto-revert-verbose nil)
  (auto-revert-interval 3)
  (global-auto-revert-non-file-buffers t)) ; Also revert dired, etc.

;;;; so-long — gracefully handle files with very long lines (e.g., minified JSON/JS)
(use-package so-long
  :ensure nil
  :hook (after-init . global-so-long-mode))

;;;; Backup and auto-save — keep backup/auto-save files out of working directories
(let ((backup-dir (expand-file-name "backups" (or (bound-and-true-p minimal-emacs-var-dir)
                                                   user-emacs-directory)))
      (auto-save-dir (expand-file-name "auto-save" (or (bound-and-true-p minimal-emacs-var-dir)
                                                        user-emacs-directory))))
  (make-directory backup-dir t)
  (make-directory auto-save-dir t)
  (setq backup-directory-alist `(("." . ,backup-dir))
        auto-save-file-name-transforms `((".*" ,auto-save-dir t))
        backup-by-copying t              ; Don't break hard links
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t))              ; Use version numbers on backups

;;;; Miscellaneous
(setq initial-major-mode 'text-mode)    ; *scratch* defaults to text-mode
(setq ispell-alternate-dictionary "/tmp/gentoo/usr/share/dict/words")
(setq text-mode-ispell-word-completion nil)
(setq explicit-shell-file-name "/bin/bash")
(setq shell-file-name "/bin/bash")
(setq process-connection-type nil)
(setq password-cache-expiry nil)
(setq shr-width 100)                   ; eww/shr rendering width
(setq use-short-answers t)             ; y/n instead of yes/no

;; ============================================================================
;; §10 Key Bindings
;; ============================================================================
;;
;; Evil SPC leader key system (Normal/Visual state, press SPC):
;;
;;   SPC SPC  switch buffer     SPC .   find file (fd)
;;   SPC ,    recent files      SPC >   find file in current dir
;;   SPC RET  eval elisp
;;
;;   SPC f    file ops          SPC b   buffer ops
;;   SPC g    Git               SPC h   help
;;   SPC s    search            SPC n   notes/Org
;;   SPC t    toggle            SPC w   window
;;   SPC S    snippets          SPC l   comment
;;   SPC o    Dired             SPC q   quit

(amos/leader
  "" nil                                ; Clear SPC prefix
  ;; Top-level shortcuts
  "SPC" '(consult-buffer :wk "buffer")
  "."   '(amos/consult-find :wk "find file")
  ">"   '(amos/consult-find-cur-dir :wk "find here")
  ","   '(consult-recent-file :wk "recent")
  "RET" '(eval-expression :wk "eval")
  "u"   '(universal-argument :wk "universal arg")
  "q"   '(:ignore t :wk "quit")
  "q q" '(save-buffers-kill-terminal :wk "quit")

  ;; [f]ile operations
  "f"   '(:ignore t :wk "file")
  "f f" 'find-file
  "f r" 'amos/rename-current-buffer-file
  "f y" 'amos/yank-buffer-filename
  "f s" 'save-buffer
  "f S" 'write-file
  "f d" 'dired-jump

  ;; Buffer — aligned with Doom layout, SPC b to switch directly
  "b"   '(consult-buffer :wk "switch buffer")
  "r"   '(amos/revert-buffer :wk "revert buffer")
  "w"   '(save-buffer :wk "save buffer")
  "i"   '(scratch-buffer :wk "scratch")
  "I"   '(amos/scratch-browse-history :wk "scratch history")

  ;; [g]it version control
  "g"   '(:ignore t :wk "git")
  "g s" 'magit-status
  "g b" 'magit-blame
  "g t" 'git-timemachine
  "g l" 'git-link
  "g r" 'git-gutter:revert-hunk
  "g p" 'git-gutter:popup-hunk
  "j"   '(git-gutter:next-hunk :wk "next hunk")
  "k"   '(git-gutter:previous-hunk :wk "prev hunk")

  ;; [h]elp system
  "h"   '(:ignore t :wk "help")
  "h f" 'describe-function
  "h v" 'describe-variable
  "h k" 'describe-key
  "h m" 'describe-mode
  "h c" 'describe-char
  "h F" 'describe-face
  "h a" 'apropos
  "h i" 'info
  "h l" 'find-library

  ;; [s]earch
  "s"   '(:ignore t :wk "search")
  "s r" 'amos/consult-ripgrep
  "s R" 'amos/consult-ripgrep-cur-dir
  "s s" 'consult-line
  "s S" 'consult-line-multi
  "s i" 'consult-imenu
  "s o" 'consult-outline
  "s d" 'deadgrep

  ;; [n]otes / playground
  "n"   '(:ignore t :wk "notes/playground")
  "n c" 'cc-playground
  "n l" 'cc-playground-find-snippet
  "n e" 'cc-playground-leetcode

  ;; [t]oggle
  "t"   '(:ignore t :wk "toggle")
  "t l" 'toggle-truncate-lines
  "t n" 'display-line-numbers-mode
  "t w" 'visual-line-mode
  "t f" 'flymake-mode
  "t t" 'consult-theme

  ;; Window management — in evil use C-w v / C-w s / C-w o etc.
  ;; Also accessible via SPC prefix
  "W"   '(:ignore t :wk "window")
  "W v" 'split-window-right
  "W s" 'split-window-below
  "W d" 'delete-window
  "W D" 'delete-other-windows
  "W o" 'other-window
  "W =" 'balance-windows

  ;; [S]nippet
  "S"   '(:ignore t :wk "snippet")
  "S n" 'yas-new-snippet
  "S i" 'yas-insert-snippet
  "S v" 'yas-visit-snippet-file

  ;; Common single-key bindings
  "l"   '(evilnc-comment-or-uncomment-lines :wk "comment")
  "e"   '(amos/shell-command-or-region :wk "shell cmd")
  "E"   '(amos/shell-command-on-buffer :wk "shell on buf")
  "R"   '(amos/revert-all-buffers :wk "revert all")
  "p"   '(project-switch-project :wk "project")
  "o"   'dired-jump)

;; C-x prefix additions
(global-set-key (kbd "C-x o") #'amos/kitty-fork-window)
(global-set-key (kbd "C-x l") #'amos/launch)

;; M-a select all
(global-set-key (kbd "M-a") #'mark-whole-buffer)

;;;; Utility functions

(defun amos/yank-buffer-filename ()
  "Copy the current file path to kill-ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name
                          (bound-and-true-p list-buffers-directory))))
      (progn (kill-new filename)
             (message "Copied: %s" filename))
    (error "Buffer is not visiting a file")))

(defun amos/yank-buffer-filename-with-line-position ()
  "Copy the current file path with line number to kill-ring."
  (interactive)
  (if-let* ((filename (or buffer-file-name
                          (bound-and-true-p list-buffers-directory))))
      (message (kill-new (concat filename ":" (number-to-string (line-number-at-pos)) "\n")))
    (error "Buffer is not visiting a file")))

(defun amos/rename-current-buffer-file ()
  "Rename the current file and its buffer."
  (interactive)
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not (and filename (file-exists-p filename)))
        (error "Buffer '%s' is not visiting a file" name)
      (let ((new-name (read-file-name "New name: " filename)))
        (rename-file filename new-name 1)
        (set-visited-file-name new-name t t)))))

(defun amos/revert-buffer ()
  "Revert the buffer without confirmation."
  (interactive)
  (revert-buffer t t)
  (message "Buffer reverted."))

;; ============================================================================
;; §11 Terminal & Server
;; ============================================================================

;;;; Terminal mouse support — enable mouse clicks and scrolling in terminal
(unless (display-graphic-p)
  (xterm-mouse-mode 1)
  ;; Terminal scroll bindings
  (global-set-key (kbd "<mouse-4>") (lambda () (interactive) (scroll-down 3)))
  (global-set-key (kbd "<mouse-5>") (lambda () (interactive) (scroll-up 3))))

;;;; Server — emacsclient support
;; Usage: emacsclient -s <server-name> <file>

(require 'server)
(setq server-name (or (getenv "EMACS_SERVER_NAME") "server"))
(unless (server-running-p server-name)
  (server-start))

;;; post-init.el ends here
