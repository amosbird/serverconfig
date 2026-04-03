;;; pre-init.el --- Pre Init -*- lexical-binding: t; -*-

;;; Package archives (minimal-emacs.d already sets MELPA/GNU/NonGNU,
;;; but we ensure our preferred order)
(setq package-archives '(("melpa"  . "https://melpa.org/packages/")
                         ("gnu"    . "https://elpa.gnu.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")))

;;; GnuTLS — relax min-prime-bits to avoid handshake failures with some ELPA hosts
(setq gnutls-min-prime-bits 256)

;;; User info
(setq user-full-name "Amos Bird")
(setq user-mail-address "amosbird@gmail.com")

;;; pre-init.el ends here
