;;; realign-mode.el --- Realign window smartly -*- lexical-binding: t; -*-
;;
;; Author: Amos Bird <amosbird@gmail.com>
;; Version: 1.0.0
;; Keywords: windows
;; URL: https://github.com/amosbird/realign.el
;; Package-Requires: ((emacs "24.4"))
;; Compatibility: GNU Emacs 24.x
;;
;; This file is NOT part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License
;; as published by the Free Software Foundation; either version 2
;; of the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;
;;; Commentary:
;;
;; Enable realign-mode and your windows are going to be realigned smartly.
;; It works fine in both GUI and terminal Emacs.
;;
;; Customizable options are:
;;  realign-lighter
;;  realign-ignore-buffer-predicates
;;  realign-hooks
;;
;;; Code:

(eval-when-compile
  (require 'cl-lib))
(require 'face-remap)
(require 'seq)
(require 'subr-x)

(defgroup realign-mode nil
  "Realign text in windows."
  :group 'windows
  :prefix "realign-")

(defcustom realign-lighter
  " |_|"
  "Mode's lighter used in the mode line."
  :group 'realign-mode
  :type 'string)

(defcustom realign-ignore-window-predicates
  (list #'realign-special-window-p)
  "List of predicate functions.
Each is run with current window and if it returns 't the
mode won't activate in that window."
  :group 'realign-mode
  :type '(list function))

(defcustom realign-need-padding-predicates
  (list #'window-full-width-p)
  "List of predicate functions.
Each is run with current window and if it returns 'nil the
left marginal area won't be padded in that window."
  :group 'realign-mode
  :type '(list function))

(defcustom realign-hooks
  nil
  "Hooks to run every time window is realigned."
  :group 'realign-mode
  :type 'hook)

(defun realign-special-window-p (window)
  "Check if WINDOW needs to be realigned."
  (let* ((buffer (window-buffer window))
         (buffname (string-trim (buffer-name buffer))))
    (or (equal buffname "*which-key*")
        (equal buffname "*mu4e-headers*")
        (equal buffname "*helm bibtex*")
        (equal (with-current-buffer buffer major-mode) 'pdf-view-mode))))

(defun realign-ignore-window-p (window)
  "Check if WINDOW should be realigned when activating the mode."
  (not (null
        (delq nil
              (mapcar (lambda (predicate) (funcall predicate window))
                      realign-ignore-window-predicates)))))

(defun realign-turn-on ()
  "Turn on window realigning."
  (add-hook 'window-configuration-change-hook #'realign-windows)
  (add-hook 'window-size-change-functions #'realign-windows)
  (add-hook 'minibuffer-setup-hook #'realign-windows)
  (realign-windows))

(defun realign-turn-off ()
  "Turn off window realigning."
  (remove-hook 'window-configuration-change-hook #'realign-windows)
  (remove-hook 'window-size-change-functions #'realign-windows)
  (remove-hook 'minibuffer-setup-hook #'realign-windows))

(defun realign-windows (&optional frame)
  "Realign all windows in FRAME, if nil, current selected frame."
  (interactive)
  (let ((windows (window-list frame t)))
    (mapc #'realign-window
          (mapcar #'realign-struct
                  (cl-remove-if #'realign-ignore-window-p windows)))
    (run-hooks 'realign-hooks)))

(cl-defstruct realign-struct
  window
  left-width
  right-width)

(defun realign-window (struct)
  "Realign given window from the STRUCT."
  (let* ((window (realign-struct-window struct)))
    (set-window-margins window
                        (realign-struct-left-width struct)
                        (realign-struct-right-width struct))))

(defun realign-struct (window)
  "Construct realign parameters for WINDOW."
  (let ((widths (realign-calculate-appropriate-margin-widths window)))
    (make-realign-struct
     :window window
     :left-width (car widths)
     :right-width (cdr widths))))

(defun realign-need-padding-p (window)
  "Check if left padding is needed for given WINDOW."
  (and (not (equal (frame-parameter nil 'name) "popup"))
       (seq-every-p (lambda (predicate) (funcall predicate window))
                    realign-need-padding-predicates)))

(defun realign-calculate-appropriate-margin-widths (window)
  "Calculate appropriate window margins for given WINDOW."
  (let* ((window-width (window-total-width window))
         (n (if (realign-need-padding-p window) (/ window-width 4) 4))
         (left-width n)
         (right-width 0))
    `(,left-width . ,right-width)))

;;;###autoload
(define-minor-mode realign-mode
  "Minor mode to center text on the current buffer"
  :init-value nil
  :global t
  :lighter realign-lighter
  (if realign-mode (realign-turn-on) (realign-turn-off)))

(provide 'realign-mode)
;;; realign-mode.el ends here
