;;; ../../../../../../home/amos/gentoo/home/amos/git/serverconfig/.config/doom/+consult.el -*- lexical-binding: t; -*-

(require 'vertico)
(require 'consult)
(require 'embark)
(require 'wgrep)
(eval-when-compile (require 'cl-lib))

(defvar-local vertico-suspend--wc nil)
(defvar-local vertico-suspend--ov nil)

(defun vertico-suspend ()
  "Suspend the current completion session.
If the command is invoked from within the Vertico minibuffer, the
current session is suspended.  If the command is invoked from
outside the minibuffer, the active minibuffer is either selected
or the latest completion session is restored."
  (interactive)
  (unless enable-recursive-minibuffers
    (user-error "Recursive minibuffers must be enabled"))
  (if-let ((win (active-minibuffer-window))
           (buf (window-buffer win))
           ((buffer-local-value 'vertico--input buf)))
      (cond
       ((minibufferp)
        (unless (frame-root-window-p win)
          (window-resize win (- (window-pixel-height win)) nil nil 'pixelwise))
        (setq vertico-suspend--ov (make-overlay (point-min) (point-max)))
        (overlay-put vertico-suspend--ov 'invisible t)
        (overlay-put vertico-suspend--ov 'priority 1000)
        (overlay-put vertico--candidates-ov 'before-string nil)
        (overlay-put vertico--candidates-ov 'after-string nil)
        (set-window-parameter win 'no-other-window t)
        (let ((list (get-buffer-window-list buf)))
          (when (length> list 1) ;; vertico-buffer
            (setq vertico-suspend--wc (current-window-configuration))
            (dolist (w list)
              (unless (eq w win)
                (delete-window w)))))
        (other-window 1))
       (t
        (select-window win)
        (set-window-parameter win 'no-other-window nil)
        (when vertico-suspend--ov
          (delete-overlay vertico-suspend--ov)
          (setq vertico-suspend--ov nil))
        (when vertico-suspend--wc
          (set-window-configuration vertico-suspend--wc nil t)
          (setq vertico-suspend--wc nil))))
    (user-error "No Vertico session to suspend or resume")))

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
                                              (leap-set-jump)
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

(defun list-interject (list separator)
  (let ((next list))
    (dotimes (_ (length (cdr list)))
      (setcdr next (cons separator (cdr next)))
      (setq next (cddr next)))))

(defun +amos-consult-fd-builder (no-ignore input)
  (pcase-let* ((`(,arg . ,opts) (consult--command-split input))
               (`(,re . ,hl) (funcall consult--regexp-compiler arg 'extended t)))
    (when re
      (list-interject re "--and")
      (cons
       (if no-ignore
           `("fd" "--color=never" "--hidden" "--full-path" "--no-ignore" ,@re ,@opts)
         `("fd" "--color=never" "--hidden" "--full-path" ,@re ,@opts))
       hl))))

(defun +amos-consult-fd (&optional dir initial no-ignore)
  (pcase-let* ((`(,prompt ,paths ,dir) (consult--directory-prompt "Fd" dir))
               (default-directory dir)
               (file (consult--find prompt (apply-partially #'+amos-consult-fd-builder no-ignore) initial)))
    (leap-set-jump)
    (find-file file)
    (recenter)))

(defun +amos-consult-find (&optional cur no-ignore)
  (if cur
      (+amos-consult-fd default-directory nil no-ignore)
    (+amos-consult-fd nil nil no-ignore)))

(defun +amos/consult-find (&optional no-ignore)
  (interactive "P")
  (+amos-consult-find nil no-ignore))

(defun +amos/consult-find-cur-dir (&optional no-ignore)
  (interactive "P")
  (+amos-consult-find t no-ignore))

(defun +amos/consult-line ()
  (interactive)
  (cl-letf (((symbol-function #'buffer-list) (lambda (&rest _) (list (current-buffer)))))
    (call-interactively #'consult-line-multi)))

(defun +amos/consult-ripgrep (&optional arg)
  (interactive "P")
  (let ((consult-ripgrep-args (concat consult-ripgrep-args (if arg " --no-ignore" ""))))
    (consult-ripgrep)))

(defun +amos/consult-ripgrep-cur-dir (&optional arg)
  (interactive "P")
  (let ((consult-ripgrep-args (concat consult-ripgrep-args (if arg " --no-ignore" ""))))
    (consult-ripgrep default-directory)))

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
  (split-string (shell-command-to-string "jump top | grep -x -v \"/\"") "\n" t))

(defun +amos/consult-jumpdir-function ()
  (interactive)
  (+amos/find-file (consult--read
                    (+amos--get-all-jump-dirs)
                    :prompt "Jump directory: "
                    :require-match t
                    :category 'file
                    :state (consult--file-preview)
                    :history 'file-name-history)))

(defmacro +amos-embark-consult-export-grep-one-line (line last-buf filename not-saved-buffers)
  `(pcase-let*
      ((`(,loc . ,num) (consult--get-location ,line))
       (lineno (format "%d" num))
       (contents (embark-consult--strip ,line))
       (this-buf (marker-buffer loc))
       (file-path (buffer-file-name this-buf)))
    (if (and file-path (not (buffer-modified-p this-buf)))
        (progn
          (unless (eq this-buf last-buf)
            (setq last-buf this-buf)
            (setq filename (file-name-nondirectory file-path))
            (insert "\n"
                    (propertize (concat "file: " filename)
                                'wgrep-ignore t
                                'font-lock-face '(:inherit compilation-info :underline t))
                    "\n"))
          (insert (propertize (concat file-path ":") 'invisible t)
                  lineno ":" contents "\n"))
      (cl-pushnew this-buf not-saved-buffers))))

(defmacro +amos-embark-consult-export-grep-one-xref (item last-buf filename not-saved-buffers)
  `(pcase-let* ((xref (get-text-property 0 'consult-xref ,item))
               (loc (xref-location-marker (xref-item-location xref)))
               (lineno (format "%d" (xref-location-line (xref-item-location xref))))
               (contents (xref-item-summary xref))
               (this-buf (marker-buffer loc))
               (file-path (buffer-file-name this-buf)))
    (if (and file-path (not (buffer-modified-p this-buf)))
        (progn
          (unless (eq this-buf last-buf)
            (setq last-buf this-buf)
            (setq filename (file-name-nondirectory file-path))
            (insert "\n"
                    (propertize (concat "file: " filename)
                                'wgrep-ignore t
                                'font-lock-face '(:inherit compilation-info :underline t))
                    "\n"))
          (insert (propertize (concat file-path ":") 'invisible t)
                  lineno ":" contents "\n"))
      (cl-pushnew this-buf not-saved-buffers))))

(defun +amos-embark-consult-export-location-grep (lines &optional xref)
  "Create a grep mode buffer listing LINES.
The elements of LINES are assumed to be values of category `consult-location'."
  (let ((buf (generate-new-buffer (embark--descriptive-buffer-name 'export))))
    (with-current-buffer buf
      (insert (propertize "Exported location results (file-backed buffers only):\n" 'wgrep-header t 'font-lock-face '(:weight bold)))
      (save-excursion (let (last-buf filename not-saved-buffers)
                        (dolist (line lines)
                          (if xref
                              (+amos-embark-consult-export-grep-one-xref line last-buf filename not-saved-buffers)
                            (+amos-embark-consult-export-grep-one-line line last-buf filename not-saved-buffers)))
                        (when not-saved-buffers
                          (save-excursion
                            (insert "\n\nSome "
                                    (propertize "buffers" 'font-lock-face '(:weight bold))
                                    " are not visiting (saved to) a file and are missing from exported results:\n")
                            (dolist (nsbuf not-saved-buffers)
                              (insert "- " (or (buffer-file-name nsbuf) (buffer-name nsbuf)) "\n"))
                            (insert "Either save the buffers or use `embark-consult-export-location-occur' as your export adapter`")
                            (message "This exporter requires the following buffers to be saved first %s" not-saved-buffers))
                          (add-text-properties (point) (point-max) '(read-only t wgrep-footer t front-sticky t)))))
      (grep-mode)

      ;; Make this buffer current for next/previous-error
      (setq next-error-last-buffer buf)
      ;; Set up keymap before possible wgrep-setup, so that wgrep
      ;; restores our binding too when the user finishes editing.
      (use-local-map (make-composed-keymap
                      embark-consult-revert-map
                      (current-local-map)))
      (setq-local wgrep-header&footer-parser #'embark-consult--wgrep-prepare)
      (when (fboundp 'wgrep-setup) (wgrep-setup)))
    (pop-to-buffer buf)))

(defun +amos-embark-consult-export-xref-grep (items)
  "Create a grep mode buffer listing ITEMS.
The elements of ITEMS are assumed to be values of category `consult-xref'."
  (+amos-embark-consult-export-location-grep items t))

(defun +amos-embark-consult-export-grep (lines)
  "Create a grep mode buffer listing LINES.
The elements of LINES are assumed to be values of category `consult-grep'."
  (let ((buf (generate-new-buffer (embark--descriptive-buffer-name 'export)))
        (count 0)
        prop)
    (with-current-buffer buf
      (insert (propertize "Exported grep results:\n\n" 'wgrep-header t 'font-lock-face '(:weight bold)) )
      (save-excursion (dolist (line lines) (insert line "\n")))
      (save-excursion
        (while (setq prop (text-property-search-forward
                           'face 'consult-highlight-match t))
          (cl-incf count)
          (put-text-property (prop-match-beginning prop)
                             (prop-match-end prop)
                             'font-lock-face
                             'match)))
      (grep-mode)
      (when (> count 0)
        (setq-local grep-num-matches-found count
                    mode-line-process grep-mode-line-matches))
      ;; Make this buffer current for next/previous-error
      (setq next-error-last-buffer buf)
      ;; Set up keymap before possible wgrep-setup, so that wgrep
      ;; restores our binding too when the user finishes editing.
      (use-local-map (make-composed-keymap
                      embark-consult-revert-map
                      (current-local-map)))
      (setq-local wgrep-header&footer-parser #'embark-consult--wgrep-prepare)
      (when (fboundp 'wgrep-setup) (wgrep-setup)))
    (pop-to-buffer buf)))

(defun +amos/embark-export-write ()
  "Export the current vertico results to a writable buffer if possible.

Supports exporting `consult-grep' to wgrep, file to wdeired, and consult-location to occur-edit"
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
        #'+amos-embark-consult-export-location-grep)

  (setf (alist-get 'consult-xref embark-exporters-alist)
        #'+amos-embark-consult-export-xref-grep)

  (setf (alist-get 'consult-grep embark-exporters-alist)
        #'+amos-embark-consult-export-grep)
  )

(remove-hook! 'consult-after-jump-hook #'+nav-flash-blink-cursor-maybe-h)
(defun consult-set-jump (&rest _)
   (leap-set-jump))
(advice-add #'consult--jump-1 :before #'consult-set-jump)
(add-hook! 'consult-after-jump-hook #'recenter)

(setq consult-buffer-sources '(consult--source-hidden-buffer consult--source-modified-buffer consult--source-buffer))

(after! consult
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
   consult--source-recent-file
   consult--source-project-recent-file
   consult--source-bookmark
   :preview-key "C-i")
  )
