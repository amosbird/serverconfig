;;; private/leap/config.el -*- lexical-binding: t; -*-

;; Author: Amos Bird <http://github.com/amosbird>
;; Maintainer: Amos Bird <amosbird@gmail.com>
;; Created: March 20, 2019
;; Modified: March 26, 2019
;; Version: 1.0.0
;; Keywords: convenience, jumplist, history
;; Homepage: https://github.com/amosbird/leap
;; Package-Requires: ((emacs "25.1"))
;;
;; This file is not part of GNU Emacs.

;;; License:
;;
;; This file is part of Leap.
;;
;; Leap is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Leap is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Leap.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;;
;; Leap is configurable jump list implementation for Emacs that can be used
;; to easily jump back to previous locations.
;;
;; To enable globally:
;;
;;     (require 'leap)
;;
;; See included README.md for more information.
;;
;;; Code:

(require 'seq)

(defgroup leap nil
  "Leap configuration options."
  :prefix "leap"
  :group 'convenience)

(defcustom leap-context 'window
  "Determines the context that leap operates within."
  :type '(choice (const :tag "Buffer" 'buffer)
                 (other :tag "Window" 'window))
  :group 'leap)

(defcustom leap-new-window-behavior 'copy
  "Determines the behavior when a new window is created."
  :type '(choice (const :tag "Empty jump list" empty)
                 (other :tag "Copy last window" copy))
  :group 'leap)

(defcustom leap-max-length 100
  "The maximum number of jumps to keep track of."
  :type 'integer
  :group 'leap)

(defcustom leap-pre-jump-hook nil
  "Hooks to run just before jumping to a location in the jump list."
  :type 'hook
  :group 'leap)

(defcustom leap-post-jump-hook nil
  "Hooks to run just after jumping to a location in the jump list."
  :type 'hook
  :group 'leap)

(defcustom leap-ignored-file-patterns '("\\*scratch\\*" "COMMIT_EDITMSG$" "TAGS$")
  "A list of regexps used to exclude files from the jump list."
  :type '(repeat string)
  :group 'leap)

(defcustom leap-disabled-modes
  '(org-agenda-mode magit-mode git-rebase-mode)
  "A list of modes in which the global leap minor mode will not be turned on."
  :group 'leap
  :type  '(list symbol))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar leap--jumping nil
  "Flag inidicating jump in progress to prevent recording unnecessary jumps.")

(defvar better-jumper--jumping nil
  "Flag inidicating jump in progress to prevent recording unnecessary jumps.")

(defvar leap--buffer-targets "\\*\\(new\\|scratch\\)\\*"
  "Regexp to match against `buffer-name' to determine whether it's a valid jump target.")

(defvar-local leap--jump-struct nil
  "Jump struct for current buffer.")

(defvar-local leap--marker-table nil
  "Marker table for current buffer.")

(cl-defstruct leap-jump-list-struct
  ring
  (idx -1)
  marker)

(defun leap--copy-struct (struct)
  "Return a copy of STRUCT."
  (let ((jump-list (leap--get-struct-jump-list struct))
        (struct-copy (make-leap-jump-list-struct)))
    (setf (leap-jump-list-struct-marker struct-copy) (leap-jump-list-struct-marker struct))
    (setf (leap-jump-list-struct-idx struct-copy) (leap-jump-list-struct-idx struct))
    (setf (leap-jump-list-struct-ring struct-copy) (ring-copy jump-list))
    struct-copy))

(defun leap--get-current-context ()
  "Get current context item. Either current window or buffer."
  (pcase leap-context
    (`buffer (current-buffer))
    (`window (frame-selected-window))))

(defun leap--set-window-struct (window struct)
  "Set jump struct for WINDOW to STRUCT."
  (set-window-parameter window 'leap-struct struct))

(defun leap--set-buffer-struct (buffer struct)
  "Set jump struct for BUFFER to STRUCT."
  (setf (buffer-local-value 'leap--jump-struct buffer) struct))

(defun leap--set-struct (context struct)
  "Set jump struct for CONTEXT to STRUCT."
  (pcase leap-context
    (`buffer (leap--set-buffer-struct context struct))
    (`window (leap--set-window-struct context struct))))

(defun leap--get-buffer-struct (&optional buffer)
  "Get current jump struct for BUFFER.
Creates and sets jump struct if one does not exist. buffer if BUFFER parameter
is missing."
  (let* ((buffer (or buffer (current-buffer)))
         (jump-struct (buffer-local-value 'leap--jump-struct buffer)))
    (unless jump-struct
      (setq jump-struct (make-leap-jump-list-struct))
      (leap--set-buffer-struct buffer jump-struct))
    jump-struct))

(defun leap--get-window-struct (&optional window)
  "Get current jump struct for WINDOW.
Creates and sets jump struct if one does not exist. buffer if WINDOW parameter
is missing."
  (let* ((window (or window (frame-selected-window)))
         (jump-struct (window-parameter window 'leap-struct)))
    (unless jump-struct
      (let ((struct (if doom--last-window (window-parameter window 'leap-struct) nil)))
        (setq jump-struct
              (if struct (leap--copy-struct struct)
                (make-leap-jump-list-struct)))
        (leap--set-struct window jump-struct)))
    jump-struct))

(defun leap--get-struct (&optional context)
  "Get current jump struct for CONTEXT.
Creates and sets jump struct if one does not exist. Uses current window or
buffer if CONTEXT parameter is missing."
  (if (eq leap-context 'buffer)
      (leap--get-buffer-struct context)
    (leap--get-window-struct context)))

(defun leap--make-key ()
  "Generate random unique key."
  (let ((key "")
        (alnum "abcdefghijklmnopqrstuvwxyz0123456789"))
    (dotimes (_ 6 key)
      (let* ((i (% (abs (random)) (length alnum))))
        (setq key (concat key (substring alnum i (1+ i))))))))

(defun leap--set-window-marker-table (window table)
  "Set marker table for WINDOW to TABLE."
  (set-window-parameter window 'leap-marker-table table))

(defun leap--set-buffer-marker-table (buffer table)
  "Set marker table for BUFFER to TABLE."
  (setf (buffer-local-value 'leap--marker-table buffer) table))

(defun leap--set-marker-table (context table)
  "Set marker table for CONTEXT to TABLE."
  (pcase leap-context
    (`buffer (leap--set-buffer-marker-table context table))
    (`window (leap--set-window-marker-table context table))))

(defun leap--get-buffer-marker-table (&optional buffer)
  "Get current marker table for BUFFER.
Creates and sets marker table if one does not exist. buffer if BUFFER parameter
is missing."
  (let* ((buffer (or buffer (current-buffer)))
         (marker-table (buffer-local-value 'leap--marker-table buffer)))
    (unless marker-table
      (setq marker-table (make-hash-table))
      (leap--set-marker-table buffer marker-table))
    marker-table))

(defun leap--get-window-marker-table (&optional window)
  "Get marker table for WINDOW.
Creates and sets marker table if one does not exist. buffer if WINDOW parameter
is missing."
  (let* ((window (or window (frame-selected-window)))
         (marker-table (window-parameter window 'leap-marker-table)))
    (unless marker-table
      (setq marker-table (make-hash-table))
      (leap--set-marker-table window marker-table))
    marker-table))

(defun leap--get-marker-table (&optional context)
  "Get current marker map for CONTEXT.
Creates and adds marker table if one does not exist. Uses current window or
buffer if CONTEXT parameter is missing."
  (if (eq leap-context 'buffer)
      (leap--get-buffer-marker-table context)
    (leap--get-window-marker-table context)))

(defun leap--get-struct-jump-list (struct)
  "Gets and potentially initialize jumps for STRUCT."
  (let ((ring (leap-jump-list-struct-ring struct)))
    (unless ring
      (setq ring (make-ring leap-max-length))
      (setf (leap-jump-list-struct-ring struct) ring))
    ring))

(defun leap--get-jump-list (&optional context)
  "Gets jump list for CONTEXT.
Uses the current context if CONTEXT is nil."
  (let ((struct (leap--get-struct context)))
    (leap--get-struct-jump-list struct)))

(defun leap--get-jump-idx (&optional context)
  "Gets jump idx for CONTEXT.
Uses the current context if CONTEXT is nil."
  (let ((struct (leap--get-struct context)))
    (leap-jump-list-struct-idx struct)))

(defun leap--jump (idx shift &optional context)
  "Jump from position IDX using SHIFT on CONTEXT.
Uses current context if CONTEXT is nil."
  (let ((jump-list (leap--get-jump-list context)))
    (setq idx (+ idx shift))
    (let* ((size (ring-length jump-list)))
      (if (>= idx size) (setq idx (- size 1)))
      (when (and (< idx size) (>= idx 0))
        ;; actual jump
        (run-hooks 'leap-pre-jump-hook)
        (let* ((marker-table (leap--get-marker-table context))
               (place (ring-ref jump-list idx))
               (file-name (nth 0 place))
               (pos (nth 1 place))
               (marker-key (nth 2 place))
               (marker (gethash marker-key marker-table)))
          (setq leap--jumping t)
          (if (string-match-p leap--buffer-targets file-name)
              (switch-to-buffer file-name)
            (find-file file-name))
          (setq leap--jumping nil)
          (if (and marker (marker-position marker))
              (goto-char marker)
            (goto-char pos)
            (puthash marker-key (point-marker) marker-table))
          (setf (leap-jump-list-struct-idx (leap--get-struct context)) idx)
          (run-hooks 'leap-post-jump-hook))))))

(defun leap--push (&optional tag context)
  "Pushes the current cursor/file position to the jump list for CONTEXT.
Uses current context if CONTEXT is nil."
  (let ((jump-list (leap--get-jump-list context))
        (marker-table (leap--get-marker-table context))
        (file-name (buffer-file-name))
        (buffer-name (buffer-name))
        (current-marker (point-marker))
        (current-point (point))
        (first-point nil)
        (first-file-name nil)
        (excluded nil))
    (when (and (not file-name)
               (string-match-p leap--buffer-targets buffer-name))
      (setq file-name buffer-name))
    (when file-name
      (dolist (pattern leap-ignored-file-patterns)
        (when (string-match-p pattern file-name)
          (setq excluded t)))
      (unless excluded
        (unless (ring-empty-p jump-list)
          (setq first-file-name (nth 0 (ring-ref jump-list 0)))
          (setq first-point (nth 1 (ring-ref jump-list 0))))
        (unless (and (equal first-point current-point)
                     (equal first-file-name file-name))
          (let ((key (leap--make-key)))
            (puthash key current-marker marker-table)
            (ring-insert jump-list `(,file-name ,current-point ,key ,tag))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;   PUBLIC FUNCTIONS    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;###autoload
(defun leap-set-jump (&optional pos)
  "Set jump point at POS.
POS defaults to point."
  (unless (or (region-active-p)
              (and (boundp 'evil-visual-state-p)
                   (evil-visual-state-p)))
    (push-mark (if (markerp pos) (marker-position pos) pos) t))

  (unless (or leap--jumping better-jumper--jumping)
    ;; clear out intermediary jumps when a new one is set
    (let* ((struct (leap--get-struct))
           (jump-list (leap--get-struct-jump-list struct))
           (idx (leap-jump-list-struct-idx struct)))
      (cl-loop repeat idx
               do (ring-remove jump-list))
      (setf (leap-jump-list-struct-idx struct) -1)
      (setf (leap-jump-list-struct-marker struct) nil))
    (save-excursion
      (when pos
        (goto-char pos))
      (leap--push))))

;;;###autoload
(defun leap-jump-backward (&optional count)
  "Jump backward COUNT positions to previous location in jump list.
If COUNT is nil then defaults to 1."
  (interactive)
  (let* ((count (or count 1))
         (struct (leap--get-struct))
         (idx (leap-jump-list-struct-idx struct))
         )
    (if (= idx -1)
        (setf (leap-jump-list-struct-marker struct) (point-marker)))
    (leap--jump idx count)))

;;;###autoload
(defun leap-jump-forward (&optional count)
  "Jump forward COUNT positions to location in jump list.
If COUNT is nil then defaults to 1."
  (interactive)
  (let* ((count (or count 1))
         (struct (leap--get-struct))
         (idx (leap-jump-list-struct-idx struct))
         (marker (leap-jump-list-struct-marker struct)))
    (unless (= idx -1)
      (if (>= (- idx count) 0)
          (leap--jump idx (- 0 count))
        (if (and marker (marker-position marker) (marker-buffer marker))
            (switch-to-buffer (marker-buffer marker))
          (goto-char marker))
        (setf (leap-jump-list-struct-idx struct) -1)
        (setf (leap-jump-list-struct-marker struct) nil)
        (run-hooks 'leap-post-jump-hook)))))

;;;###autoload
(defun leap-get-jumps (window-or-buffer)
  "Get jumps for WINDOW-OR-BUFFER.
The argument should be either a window or buffer depending on the context."
  (let* ((struct (leap--get-struct window-or-buffer))
         (struct-copy (leap--copy-struct struct)))
    struct-copy))

;;;###autoload
(defun leap-set-jumps (window-or-buffer jumps)
  "Set jumps to JUMPS for WINDOW-OR-BUFFER.
The argument should be either a window or buffer depending on the context."
  (let ((struct-copy (leap--copy-struct jumps)))
    (leap--set-struct window-or-buffer struct-copy)))

;;;###autoload
(defun leap-clear-jumps ()
  "Set jumps to JUMPS for WINDOW-OR-BUFFER.
The argument should be either a window or buffer depending on the context."
  (leap--set-struct (frame-selected-window) (make-leap-jump-list-struct)))

(push '(leap-struct . writable) window-persistent-parameters)

(provide 'leap)
;;; leap.el ends here
