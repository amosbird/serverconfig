;;;  -*- lexical-binding: t; -*-
(setq doom-font (font-spec :family "Ubuntu Mono" :size 18)
      doom-line-numbers-style 'relative
      ;; fix start-process xdg-open
      process-connection-type nil)

(add-hook! 'eval-expression-minibuffer-setup-hook
  (define-key minibuffer-local-map "\C-p" #'previous-line-or-history-element)
  (define-key minibuffer-local-map "\C-n" #'next-line-or-history-element))

(remove-hook! 'doom-popup-mode-hook #'doom|hide-modeline-in-popup)
(advice-add #'nlinum-mode :override #'ignore)
(advice-add #'doom-hide-modeline-mode :override #'ignore)
(advice-add #'fringe-mode :override #'ignore)
(advice-add #'dired-k--highlight-by-file-attribyte :override #'ignore)
(advice-add #'recenter-top-bottom :override #'recenter)
(advice-add #'git-gutter:next-hunk :after (lambda (arg) (recenter)))
(def-package-hook! git-gutter-fringe :disable)
(def-package-hook! smerge-mode :disable)
(def-package-hook! solaire-mode :disable)
(def-package-hook! stripe-buffer :disable)
(def-package-hook! visual-fill-column :disable)

(def-package-hook! nav-flash
  :pre-init
  (advice-add #'windmove-do-window-select :after #'+nav-flash/blink-cursor)
  (advice-add #'recenter :after #'+nav-flash/blink-cursor)
  (after! evil
    (advice-add #'evil--jumps-jump :after (lambda (&rest _) (recenter)))
    (advice-add #'evil-switch-to-windows-last-buffer :after (lambda (&rest _) (recenter))))
  nil)

(after! subword
  (progn
    (define-category ?U "Uppercase")
    (define-category ?u "Lowercase")
    (modify-category-entry (cons ?A ?Z) ?U)
    (modify-category-entry (cons ?a ?z) ?u)
    (make-variable-buffer-local 'evil-cjk-word-separating-categories)
    (add-hook 'subword-mode-hook (lambda! (if subword-mode (push '(?u . ?U) evil-cjk-word-separating-categories)
                                        (setq evil-cjk-word-separating-categories (default-value 'evil-cjk-word-separating-categories)))))))

;; (def-package-hook! evil-snipe :disable)
;; (def-package-hook! ivy-xref
;;   :post-config (setq xref-show-xrefs-function #'+amos/ivy-xref-show-xrefs)
;;   t)

(def-package-hook! company
  :post-config
  (require 'company-tng)
  (push 'company-tng-frontend company-frontends)
  (defvar-local company-fci-mode-on-p nil)
  (defun company-turn-off-fci (&rest ignore)
    (when (boundp 'fci-mode)
      (setq company-fci-mode-on-p fci-mode)
      (when fci-mode (fci-mode -1))))
  (defun company-maybe-turn-on-fci (&rest ignore)
    (when company-fci-mode-on-p (fci-mode 1)))
  (add-hook 'company-completion-started-hook 'company-turn-off-fci)
  (add-hook 'company-completion-finished-hook 'company-maybe-turn-on-fci)
  (add-hook 'company-completion-cancelled-hook 'company-maybe-turn-on-fci)
  t)

(def-package-hook! magit
  :post-config
  (setq
   magit-display-buffer-function 'magit-display-buffer-fullframe-status-topleft-v1
   magit-display-buffer-noselect t
   magit-revision-show-gravatars '("^Author:     " . "^Commit:     "))
  t)

(require 'server)
(setq server-name (getenv "EMACS_SERVER_NAME"))
(if (not server-name) (setq server-name "server"))
(unless (server-running-p server-name)
  (server-start))
;; disable this fucking stupid feature by masking
(provide 'smartparens-lua)
