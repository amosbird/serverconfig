;;; pre-early-init.el --- Pre Early Init -*- lexical-binding: t; -*-

;;; Disable UI components — minimal-emacs.d controls menu/tool/scroll bar via this variable
(setq minimal-emacs-ui-features '())

;;; Startup optimization — suppress redisplay to prevent unconfigured UI flicker
(setq minimal-emacs-inhibit-redisplay-during-startup t)

;;; Terminal input — restore Kitty keyboard flags after dtach sends SIGWINCH on reattach
(defun amos/restore-kitty-keyboard-mode (frame)
  "Restore Emacs's Kitty keyboard flags on the reattached TTY for FRAME."
  (when (and (not (display-graphic-p frame))
             (fboundp 'kitty-keyboard-mode-active-p))
    (when-let* ((flags (kitty-keyboard-mode-active-p (frame-terminal frame))))
      ;; Set flags in place: unlike CSI > u, this does not grow Kitty's mode stack.
      (send-string-to-terminal (format "\e[=%d;1u" flags) (frame-terminal frame)))))
(add-hook 'window-size-change-functions #'amos/restore-kitty-keyboard-mode)

;;; Font
(push '(font . "Ubuntu Mono-17") default-frame-alist)

;;; Frame parameters
(push '(internal-border-width . 0) default-frame-alist)
(push '(undecorated . nil) default-frame-alist)

;;; Packages — keep package.el state inside this init directory
(setq package-user-dir (expand-file-name "elpa" user-emacs-directory))

;;; Redirect state files to var/ to keep ~/.abemacs clean
(defvar minimal-emacs-var-dir
  (expand-file-name "var/" minimal-emacs-user-directory))
(unless (file-directory-p minimal-emacs-var-dir)
  (make-directory minimal-emacs-var-dir t))

;;; pre-early-init.el ends here
