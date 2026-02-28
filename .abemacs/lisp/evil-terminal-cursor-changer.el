;;; evil-terminal-cursor-changer.el --- Change cursor shape and color by evil state in terminal  -*- coding: utf-8; -*-
;; Package-Version: 20171008.1
;; Version: 0.4
;;
;; evil-terminal-cursor-changer is changing cursor shape and color by evil state for evil-mode.
;;
;; Simple architecture (same as Doom):
;;   defadvice on evil-set-cursor reads cursor-type after each call
;;   and sends the corresponding DECSCUSR escape sequence.
;;   No caching, no timers, no hooks — just direct defadvice.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or
;; (at your option) any later version.
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;; Code:

(require 'evil)

(defun etcc--make-cursor-shape-seq (shape)
  "Make escape sequence SHAPE for XTerm."
  (if (listp shape) (setq shape (car shape)))
  (let ((cs (cond ((eq shape 'box) "2")
                  ((eq shape 'hbar) "4")
                  ((eq shape 'bar) "6")
                  (t nil))))
    (if cs (concat "\e[" cs " q") nil)))

(defun etcc--make-cursor-color-seq (color)
  "Make escape sequence COLOR for cursor color."
  (if color (concat "\e]12;" color "\007")))

(defun etcc--apply-to-terminal (seq)
  "Send escape sequence SEQ to terminal."
  (when (and seq (stringp seq))
    (send-string-to-terminal seq)))

(defadvice evil-set-cursor-color (after etcc--evil-set-cursor-color (color))
  "Advice `evil-set-cursor-color'."
  (unless window-system
    (etcc--apply-to-terminal (etcc--make-cursor-color-seq color))))

(defadvice evil-set-cursor (after etcc--evil-set-cursor)
  "Advice `evil-set-cursor'."
  (unless window-system
    (etcc--apply-to-terminal (etcc--make-cursor-shape-seq cursor-type))))

;;;###autoload
(defun evil-terminal-cursor-changer-activate ()
  "Enable evil terminal cursor changer."
  (interactive)
  (ad-activate 'evil-set-cursor-color)
  (ad-activate 'evil-set-cursor))

;;;###autoload
(defalias 'etcc-on 'evil-terminal-cursor-changer-activate)

;;;###autoload
(defun evil-terminal-cursor-changer-deactivate ()
  "Disable evil terminal cursor changer."
  (interactive)
  (ad-deactivate 'evil-set-cursor-color)
  (ad-deactivate 'evil-set-cursor))

;;;###autoload
(defalias 'etcc-off 'evil-terminal-cursor-changer-deactivate)

(provide 'evil-terminal-cursor-changer)

;;; evil-terminal-cursor-changer.el ends here
