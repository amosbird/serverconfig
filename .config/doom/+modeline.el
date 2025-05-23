(after! doom-modeline
  (defface +amos-workspace-tab-selected-face '((t (:inherit 'highlight))) "." :group 'amos-face)
  (defface +amos-workspace-tab-face '((t (:foreground "#bbc2cf" :inherit 'mode-line))) "." :group 'amos-face)

  (setq +amos-full-width-digits ["１" "２" "３" "４" "５" "６" "７" "８" "９"])
  (defun +amos-frame-modeline (&optional _)
    (let ((frames (tty-root-frame-list))
          (current-frame (selected-root-frame)))
      (concat " |"
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
               "|")
              "|")))
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

  ;; (add-hook! 'pre-command-hook :append #'keycast-mode-line-update)
  ;; (define-minor-mode keycast-mode-line-mode
  ;;   "Show current command and its key binding in the mode line."
  ;;   :global t
  ;;   :lighter " Key"
  ;;   (if keycast-mode-line-mode
  ;;       (add-hook! 'pre-command-hook :append #'keycast-mode-line-update)
  ;;     (remove-hook! 'pre-command-hook #'keycast-mode-line-update)))

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

  (setq +amos-system-name
        (let ((hostname (getenv "HOSTNAME")))
          (if (and hostname (not (string-empty-p hostname)))
              hostname
            (system-name))))
  (defvar +amos--hostname (propertize (concat "  " +amos-system-name " ") 'face '(:weight bold :foreground "#51afef")))
  (doom-modeline-def-segment host (let () +amos--hostname))

  ;; (defun +amos*doom-modeline-set-modeline (&rest _)
  ;;   (setq-default global-mode-line-format (list "%e" (doom-modeline 'amos)))
  ;;   (setq mode-line-format '((:eval (+amos-modeline-horizontal-sep))))
  ;;   (setq-default mode-line-format '((:eval (+amos-modeline-horizontal-sep)))))
  ;; (advice-add #'doom-modeline-set-modeline :override #'+amos*doom-modeline-set-modeline)

  (defun +amos*doom-modeline-set-modeline (&rest _)
    (setq mode-line-format (list "%e" (doom-modeline 'amos)))
    (setq-default mode-line-format (list "%e" (doom-modeline 'amos))))
  (advice-add #'doom-modeline-set-modeline :override #'+amos*doom-modeline-set-modeline)

  ;; ignore window-font-height which will call select-window which calls evil-set-curosr
  (defun +amos-window-font-height-a (&rest _) 1)
  (advice-add #'window-font-height :override #'+amos-window-font-height-a)
  (setq after-focus-change-function #'ignore) ; it doens't make sense to update modeline
  )

(defun +amos-modeline-horizontal-sep()
  (let* ((width (window-pixel-width)))
    (make-string width ?─)))

(defun +amos-string-pixel-width (str)
  "Return the width of STR in pixels."
  (* (string-width str) (window-font-width nil 'mode-line)))

(defun doom-modeline-format--amos ()
  (let ((lhs-forms (doom-modeline--prepare-segments
                    '(
                      bar
                      matches
                      follow
                      buffer-info buffer-position word-count parrot selection-info
                      frame
                      )))
        (rhs-forms (doom-modeline--prepare-segments
                    '(;; keycast
                      ;; host
                      lsp
                      indent-info buffer-encoding major-mode process vcs check time
                      ))))
    (list lhs-forms
          (let* (
                 (lhs-str (format-mode-line (cons "" lhs-forms) nil nil (current-buffer)))
                 (lhs-len (length lhs-str))
                 (lhs-width (progn
                              (add-face-text-property
                               0 lhs-len 'mode-line t lhs-str)
                              (+amos-string-pixel-width lhs-str)))
                 (rhs-str (format-mode-line (cons "" rhs-forms)  nil nil (current-buffer)))
                 (rhs-len (length rhs-str))
                 (rhs-width (progn
                              (add-face-text-property
                               0 rhs-len 'mode-line t rhs-str)
                              (+amos-string-pixel-width rhs-str)))
                 )
            (propertize (make-string (- (frame-pixel-width) lhs-width rhs-width) ?-)
                        'face `(:foreground ,(face-background 'mode-line))))
          rhs-forms
          ))
  )
