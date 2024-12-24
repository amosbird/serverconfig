;;; ../../../../../../home/amos/gentoo/home/amos/git/serverconfig/.config/doom/+consult.el -*- lexical-binding: t; -*-

(require 'vertico)
(require 'consult)
(require 'embark)
(require 'wgrep)
(eval-when-compile (require 'cl-lib))

(defun amos-consult--yank-by (fn &rest args)
  "Pull buffer text from current line into search string.
The region to extract is determined by the respective values of
point before and after applying FN to ARGS."
  (let (text)
    (with-minibuffer-selected-window
      (let ((beg (point))
            (bol (line-beginning-position))
            (eol (line-end-position))
            end)
        (unwind-protect
            (progn (apply fn args)
                   (setq end (goto-char (max bol (min (point) eol))))
                   (setq text (buffer-substring-no-properties beg end))
                   (amos-consult--pulse-region beg end))
          (unless text
            (goto-char beg)))))
    (when text
      (insert (replace-regexp-in-string "  +" " " text t t)))))

(defun +amos/consult-yank-word (&optional arg)
  "Pull next word from buffer into search string.
If optional ARG is non-nil, pull in the next ARG
words (previous if ARG is negative)."
  (interactive "p")
  (amos-consult--yank-by #'forward-word arg))

(defun +amos/consult-yank-symbol (&optional arg)
  "Pull next symbol from buffer into search string.
If optional ARG is non-nil, pull in the next ARG
symbols (previous if ARG is negative)."
  (interactive "p")
  (amos-consult--yank-by #'forward-symbol (or arg 1)))

(defun +amos/consult-yank-char (&optional arg)
  "Pull next character from buffer into search string.
If optional ARG is non-nil, pull in the next ARG
characters (previous if ARG is negative)."
  (interactive "p")
  (amos-consult--yank-by #'forward-char arg))

(defvar amos-consult--pulse-overlay nil
  "Overlay used to highlight yanked word.")

(defvar amos-consult--pulse-timer nil
  "Timer used to dispose of `amos-consult--pulse-overlay'.")

(defcustom amos-consult-pulse-delay 0.5
  "Number of seconds to display `amos-consult-yanked-word' highlight.
When nil, disable highlighting."
  :type '(choice
          (number :tag "Delay in seconds")
          (const :tag "Disable" nil)))

(defface amos-consult-yanked-word
  '((t :inherit highlight))
  "Face used to highlight yanked word.")

(defun amos-consult--pulse-region (start end)
  "Temporarily highlight text between START and END.
The \"pulse\" duration is determined by `amos-consult-pulse-delay'."
  (when amos-consult-pulse-delay
    (if amos-consult--pulse-overlay
        (let ((ostart (overlay-start amos-consult--pulse-overlay))
              (oend (overlay-end amos-consult--pulse-overlay)))
          (when (< end start)
            (cl-rotatef start end))
          ;; Extend the existing overlay's region to include START..END,
          ;; but only if the two regions are contiguous.
          (move-overlay amos-consult--pulse-overlay
                        (if (= start oend) ostart start)
                        (if (= end ostart) oend end)))
      (setq amos-consult--pulse-overlay (make-overlay start end))
      (overlay-put amos-consult--pulse-overlay 'face 'amos-consult-yanked-word))
    (when amos-consult--pulse-timer
      (cancel-timer amos-consult--pulse-timer))
    (setq amos-consult--pulse-timer
          (run-at-time amos-consult-pulse-delay nil #'amos-consult--pulse-cleanup))))

(defun amos-consult--pulse-cleanup ()
  "Cancel `amos-consult--pulse-timer' and delete `amos-consult--pulse-overlay'."
  (when amos-consult--pulse-timer
    (cancel-timer amos-consult--pulse-timer)
    (setq amos-consult--pulse-timer nil))
  (when amos-consult--pulse-overlay
    (delete-overlay amos-consult--pulse-overlay)
    (setq amos-consult--pulse-overlay nil)))

(defun +amos/embark-collect ()
  "Create an Embark Collect buffer."
  (interactive)
  (embark-collect))

(defun +amos-consult-recentf (&optional no-cache)
  (when no-cache
    (require 'recentf)
    (recentf-cleanup))
  (cl-letf* ((+amos-find-file (symbol-function #'find-file))
             ((symbol-function #'find-file) (lambda (&rest args)
                                              (better-jumper-set-jump)
                                              (apply +amos-find-file args)
                                              (recenter))))
    (let ((recentf-list (--remove (string= it sync-recentf-marker) recentf-list)))
      (consult-recent-file))))

(defun +amos/consult-recentf ()
  (interactive)
  (+amos-consult-recentf))

(defun +amos/consult-recentf-no-cache ()
  (interactive)
  (+amos-consult-recentf t))

(defun +amos-consult-find (&optional cur no-ignore)
  (let* ((consult-fd-args-base '("fd" "--color=never" "--hidden"))
         (consult-fd-args (if no-ignore (append consult-fd-args-base '("--no-ignore")) consult-fd-args-base)))
    (cl-letf* ((+amos-find-file (symbol-function #'find-file))
               ((symbol-function #'find-file) (lambda (&rest args)
                                                (better-jumper-set-jump)
                                                (apply +amos-find-file args)
                                                (recenter))))
      (consult-fd cur nil))))

(defun +amos/consult-find (&optional no-ignore)
  (interactive "P")
  (+amos-consult-find nil no-ignore))

(defun +amos/consult-find-cur-dir (&optional no-ignore)
  (interactive "P")
  (+amos-consult-find default-directory no-ignore))

(defun +amos/consult-line ()
  (interactive)
  (cl-letf (((symbol-function #'buffer-list) (lambda (&rest _) (list (current-buffer)))))
    (call-interactively #'consult-line-multi)))

;; consult-ripgrep uses consult--jump instead of find-file
(defun +amos*consult--jump (orig-fun pos)
  (when pos
    (better-jumper-set-jump)
    (funcall orig-fun pos)))
(advice-add #'consult--jump :around #'+amos*consult--jump)
(add-hook! 'consult-after-jump-hook #'recenter)
(defun +amos-consult-ripgrep (&optional cur)
  (cl-letf* ((+amos-find-file (symbol-function #'find-file))
             ((symbol-function #'find-file) (lambda (&rest args)
                                              (better-jumper-set-jump)
                                              (apply +amos-find-file args)
                                              (recenter))))
    (consult-ripgrep cur)))

(defun +amos/consult-ripgrep (&optional arg)
  (interactive "P")
  (let ((consult-ripgrep-args (concat consult-ripgrep-args (if arg " --no-ignore" ""))))
    (+amos-consult-ripgrep)))

(defun +amos/consult-ripgrep-cur-dir (&optional arg)
  (interactive "P")
  (let ((consult-ripgrep-args (concat consult-ripgrep-args (if arg " --no-ignore" ""))))
    (+amos-consult-ripgrep default-directory)))

(defun +amos-git-link (cand)
  (require 'git-link)
  (cl-destructuring-bind (remote branch) (split-string cand "~")
    (let ((git-link-default-remote remote)
          (git-link-default-branch branch))
      (call-interactively #'git-link))))

(defun +amos/consult-git-link ()
  (interactive)
  (let ((l (split-string (string-trim-right (shell-command-to-string "gittrackedremote")) "\n")))
    (+amos-git-link
     (if (< (length l) 2)
         (car l)
       (consult--read
        l
        :prompt "Git link: "
        :require-match t)))))

(defun +amos/consult-jumpfile-function ()
  (interactive)
  (+amos/find-file (consult--read
                    (directory-files-recursively default-directory ".*")
                    :prompt "Jump file: "
                    :require-match t
                    :category 'file
                    :state (consult--file-preview)
                    :history 'file-name-history)))

(defun +amos--get-all-jump-dirs ()
  (unless (file-directory-p default-directory)
    (cd "~"))
  (split-string (shell-command-to-string "jump-top | grep -x -v \"/\"") "\n" t))

(defun +amos/consult-jumpdir-function ()
  (interactive)
  (+amos/find-file (consult--read
                    (+amos--get-all-jump-dirs)
                    :prompt "Jump directory: "
                    :require-match t
                    :category 'file
                    :state (consult--file-preview)
                    :history 'file-name-history)))

;; Forked from modules/completion/vertico/autoload/vertico.el
(defun +amos/embark-export-write ()
  "Export vertico results to a writable buffer if possible.
Supports exporting `consult-grep' to wgrep, file to wdeired, and
consult-location to occur-edit."
  (interactive)
  (let* ((edit-command
          (pcase-let ((`(,type . ,candidates)
                       (run-hook-with-args-until-success 'embark-candidate-collectors)))
            (pcase type
              ('consult-grep #'wgrep-change-to-wgrep-mode)
              ('file #'wdired-change-to-wdired-mode)
              ('consult-location #'wgrep-change-to-wgrep-mode)
              ('consult-xref #'wgrep-change-to-wgrep-mode)
              (x (user-error "Embark category %S doesn't support writable export" x)))))
         (embark-after-export-hook `(,@embark-after-export-hook ,edit-command)))
    (cl-letf (((symbol-function #'embark--descriptive-buffer-name)
               (lambda (&rest _)
                 (format "*Embark Writable Export: %s*"
                         (if (minibufferp)
                             (format "%s - %s" embark--command
                                     (minibuffer-contents-no-properties))
                           (buffer-name))))))
      (embark-export))))

(after! embark-consult
  (setf (alist-get 'consult-location embark-exporters-alist)
        #'embark-consult-export-location-grep)

  (setf (alist-get 'consult-xref embark-exporters-alist)
        #'embark-consult-export-xref-grep)
  )

(setq consult-buffer-sources '(consult--source-hidden-buffer consult--source-modified-buffer consult--source-buffer))

(after! consult
  ; TODO lookups doesn't work after emacs 30
  (setq consult-preview-key "C-i")
  (consult-customize
   consult-buffer
   consult-ripgrep
   consult-git-grep
   consult-grep
   consult-bookmark
   consult-recent-file
   +amos/consult-jumpfile-function
   +amos/consult-jumpdir-function
   +amos/consult-recentf
   +amos/consult-recentf-no-cache
   +amos/consult-find
   +amos/consult-find-cur-dir
   +amos/consult-line
   +amos/consult-ripgrep
   +amos/consult-ripgrep-cur-dir
   +lookup/definition
   +lookup/references
   +lookup/implementations
   +lookup/type-definition
   +lookup/documentation
   +lookup/file
   consult--source-recent-file
   consult--source-project-recent-file
   consult--source-bookmark
   :preview-key "C-i")
  )
