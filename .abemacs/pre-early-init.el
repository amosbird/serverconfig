;;; pre-early-init.el --- Pre Early Init -*- lexical-binding: t; -*-

;;; Disable UI components — minimal-emacs.d controls menu/tool/scroll bar via this variable
(setq minimal-emacs-ui-features '())

;;; Startup optimization — suppress redisplay to prevent unconfigured UI flicker
(setq minimal-emacs-inhibit-redisplay-during-startup t)

;;; Font
(push '(font . "Ubuntu Mono-17") default-frame-alist)

;;; Frame parameters
(push '(internal-border-width . 0) default-frame-alist)
(push '(undecorated . nil) default-frame-alist)

;;; Redirect state files to var/ to keep ~/.emacs.d clean
(defvar minimal-emacs-var-dir
  (expand-file-name "var/" minimal-emacs-user-directory))
(unless (file-directory-p minimal-emacs-var-dir)
  (make-directory minimal-emacs-var-dir t))

;;; pre-early-init.el ends here
