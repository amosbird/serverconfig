;;; ui/doom-modeline/config.el -*- lexical-binding: t; -*-

(require 'doom-modeline)

(defface +amos-workspace-tab-selected-face '((t (:inherit 'highlight))) "." :group 'amos-face)
(defface +amos-workspace-tab-face '((t (:foreground "#bbc2cf" :inherit 'doom-modeline-highlight))) "." :group 'amos-face)

(setq +amos-full-width-digits ["１" "２" "３" "４" "５" "６" "７" "８" "９"])
(defun +amos-frame-modeline (&optional _)
  (let ((frames +amos-frame-list)
        (current-frame (selected-frame)))
    (concat (propertize " |" 'face '+amos-workspace-tab-face)
            (mapconcat
             #'identity
             (cl-loop for frame in frames
                      for i to (length frames)
                      collect
                      (propertize
                       (if (< i 9)
                           (elt +amos-full-width-digits i)
                         (format "%d" (1+ i)))
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
  (setq keycast--this-command-keys (this-command-keys))
  (setq keycast--this-command this-command))

(add-hook! 'pre-command-hook :append #'keycast-mode-line-update)

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

;; add padding for short buffer name
(doom-modeline-def-segment amos-matches
  (let ((meta (concat (doom-modeline--macro-recording)
                      (doom-modeline--anzu)
                      (doom-modeline--evil-substitute)
                      (doom-modeline--iedit)
                      (doom-modeline--symbol-overlay)
                      (doom-modeline--multiple-cursors))))
    (concat (if (not buffer-file-name) (make-string 24 ?\ ))
            (or (and (not (equal meta "")) meta)
                " %I "))))

(defvar +amos--hostname (propertize (concat "  " (system-name) " ") 'face '(:weight bold :foreground "#51afef")))
(doom-modeline-def-segment host (let () +amos--hostname))

(doom-modeline-def-modeline 'amos
  '(bar amos-matches buffer-info buffer-position selection-info frame)
  '(keycast host buffer-encoding major-mode vcs checker))

(defun +amos-setup-custom-doom-modeline-h ()
  (doom-modeline-set-modeline 'amos 'default)
  (with-current-buffer "*Messages*" (doom-modeline-set-modeline 'amos))
  (advice-add #'doom-modeline-set-modeline :override #'ignore))

(add-hook! 'doom-modeline-mode-hook #'+amos-setup-custom-doom-modeline-h)
