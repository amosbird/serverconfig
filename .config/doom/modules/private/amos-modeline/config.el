;;; ui/doom-modeline/config.el -*- lexical-binding: t; -*-

(require 'doom-modeline)

(defvar +amos--hostname (propertize (system-name) 'face '(:weight bold :foreground "#51afef")))

(doom-modeline-def-segment host +amos--hostname)

(defface +amos-workspace-tab-selected-face '((t (:inherit 'highlight))) "." :group 'amos-face)
(defface +amos-workspace-tab-face '((t (:inherit 'default))) "." :group 'amos-face)
(defun +amos-frame-modeline (&optional _)
  (let ((frames +amos-frame-list)
        (current-frame (selected-frame)))
    (concat (propertize "|" 'face '+amos-workspace-tab-face)
            (mapconcat
             #'identity
             (cl-loop for frame in frames
                      for i to (length frames)
                      collect
                      (propertize (format " %d " (1+ i))
                                  'face (if (eq current-frame frame)
                                            '+amos-workspace-tab-selected-face
                                          '+amos-workspace-tab-face)))
             (propertize "|" 'face '+amos-workspace-tab-face))
            (propertize "|" 'face '+amos-workspace-tab-face))))
(doom-modeline-def-segment frame (+amos-frame-modeline))

(defface keycast-key
  '((t (:weight bold
                :height 1.0
                :background "#d5cfbf"
                :foreground "#000000"
                :box (:line-width -3 :style released-button))))
  "When Keycast mode is enabled, face used for the key in the mode line."
  :group 'keycast)

(defface keycast-command '((t (:weight bold)))
  "When Keycast mode is enabled, face used for the command in the mode line."
  :group 'keycast)

(defvar keycast--this-command nil)
(defvar keycast--this-command-keys nil)

(defun keycast-mode-line-update ()
  "Update mode line with current `this-command' and `this-command-keys'."
  ;; Remember these values because the mode line update won't actually
  ;; happen until we return to the command loop and by that time these
  ;; values have been reset to nil.
  (setq keycast--this-command-keys (this-command-keys))
  (setq keycast--this-command this-command))

(add-hook 'pre-command-hook 'keycast-mode-line-update t)

(doom-modeline-def-segment keycast
  (let* ((key (ignore-errors
                (key-description keycast--this-command-keys)))
         (cmd keycast--this-command))
    (or
     (and key cmd
          (concat
           (make-string 10 ?\s)
           (propertize (let ((pad (max 2 (- 5 (length key)))))
                         (concat (make-string (ceiling pad 2) ?\s) key
                                 (make-string (floor   pad 2) ?\s)))
                       'face 'keycast-key)
           (format " %s" (propertize (if (symbolp cmd) (symbol-name cmd) "No Key")
                                     'face 'keycast-command)))))))

(defun +amos-buffer-file-name ()
  "~/Projects/FOSS/emacs/lisp/comint.el => ~/P/F/emacs/l/comint.el"
  (let* ((project-root (or (doom-project-root) ""))
         (file-name-split (shrink-path-file-mixed project-root
                                                  (file-name-directory buffer-file-name)
                                                  buffer-file-name))
         (active t))
    (if (null file-name-split)
        (propertize "%b" 'face (if active 'doom-modeline-buffer-file))
      (pcase-let ((`(,root-path-parent ,project ,relative-path ,filename) file-name-split))
        (let ((modified-faces (if (buffer-modified-p) 'doom-modeline-buffer-modified)))
          (let ((sp-faces       (or modified-faces (if active 'font-lock-comment-face)))
                (project-faces  (or modified-faces (if active 'font-lock-string-face)))
                (relative-faces (or modified-faces (if active 'doom-modeline-buffer-path)))
                (file-faces     (or modified-faces (if active 'doom-modeline-buffer-file))))
            (let* ((sp-props       `(,@(if sp-faces       `(:inherit ,sp-faces))      ,@(if active '(:weight bold))))
                   (project-props  `(,@(if project-faces  `(:inherit ,project-faces)) ,@(if active '(:weight bold))))
                   (relative-props `(,@(if relative-faces `(:inherit ,relative-faces))))
                   (file-props     `(,@(if file-faces     `(:inherit ,file-faces))))
                   (line (concat (propertize root-path-parent 'face sp-props)
                                 (propertize (concat project "/") 'face project-props))))
              (if (and relative-path (> (+ (length line) (length filename) (length relative-path)) 60))
                  (setq relative-path (string-remove-prefix "/" (shrink-path--dirs-internal relative-path t))))
              (concat line
                      (if relative-path (propertize relative-path 'face relative-props))
                      (propertize filename 'face file-props)))))))))

(doom-modeline-def-segment amos-buffer-info
  "Combined information about the current buffer, including the current working
directory, the file name, and its state (modified, read-only or non-existent)."
  (if buffer-file-name
      (+amos-buffer-file-name)
    (buffer-name)))

(doom-modeline-def-segment amos-matches
  "Displays: 1. the currently recording macro, 2. A current/total for the
current search term (with anzu), 3. The number of substitutions being conducted
with `evil-ex-substitute', and/or 4. The number of active `iedit' regions."
  (let ((meta (concat (doom-modeline--macro-recording)
                      (doom-modeline--anzu)
                      (doom-modeline--evil-substitute)
                      (doom-modeline--iedit)
                      (doom-modeline--symbol-overlay)
                      (doom-modeline--multiple-cursors))))
    (concat (if (not buffer-file-name) (make-string 20 ?\ ))
            (or (and (not (equal meta "")) meta)
                " %I "))))

(doom-modeline-def-segment amos-evil-state
  (evil-state-property evil-state :name))

(doom-modeline-def-segment amos-lcp
  ""
  " %l:%c %p ")

(doom-modeline-def-modeline 'amos
  '(" " amos-matches " " amos-buffer-info amos-lcp selection-info frame)
  '(" " keycast "  " host "  " buffer-encoding major-mode vcs checker))
  ;; '(bar matches buffer-info remote-host buffer-position parrot selection-info)
  ;; '(misc-info minor-modes input-method buffer-encoding major-mode process vcs checker))

(defun +amos|setup-custom-doom-modeline ()
  (doom-modeline-set-modeline 'amos 'default))

(add-hook 'doom-modeline-mode-hook '+amos|setup-custom-doom-modeline)
(advice-add #'doom-modeline-set-pdf-modeline :override #'ignore)
(advice-add #'doom-modeline-set-special-modeline :override #'ignore)
(advice-add #'doom-modeline-set-media-modeline :override #'ignore)
(advice-add #'doom-modeline-set-project-modeline :override #'ignore)
