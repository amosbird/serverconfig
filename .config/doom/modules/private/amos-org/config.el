;;; org/org/config.el -*- lexical-binding: t; -*-

;; Custom variables
(defvar +org-dir (expand-file-name "~/org/")
  "The directory where org files are kept.")

(after! evil-org
  (add-hook! org-mode (evil-org-mode))
  (evil-org-set-key-theme '(navigation textobjects))

  (after! org-agenda
    (setq-default
     org-agenda-dim-blocked-tasks nil
     org-agenda-inhibit-startup t
     org-agenda-skip-unavailable-files t)
    (require 'evil-org-agenda)
    (evil-org-agenda-set-keys))

  (map! (:map (evil-org-mode-map)
          "<f8>"             #'+amos/insert-todo-header
          :gniv [S-return]   #'+amos/insert-todo-header
          :gniv [C-return]   #'+amos/insert-header
          :gniv [M-return]   #'+amos/org-meta-return
          :gniv "M-RET"      #'+amos/org-meta-return
          :gniv "C-t"        #'+amos/org-todo
          :n "C-i"           #'better-jumper-jump-forward
          :g "C-c H"         #'org-do-promote
          :g "C-c L"         #'org-do-demote
          :g "C-c K"         #'org-promote-subtree
          :g "C-c J"         #'org-demote-subtree
          :g "C-c C-j"       #'org-metadown
          :g "C-c C-k"       #'org-metaup
          :g "C-c x"         #'org-cut-subtree
          :g "C-c y"         #'org-copy-subtree
          :g "C-c p"         #'org-paste-subtree
          :g "C-c r"         #'org-refile
          :g "C-c s"         #'org-sort
          :g "C-c t"         #'org-toggle-heading
          :n "gt"            #'evil-struct-state
          :n "RET"           #'org-open-at-point
          :n "M-h"           #'evil-window-left
          :n "M-j"           #'evil-window-down
          :n "M-k"           #'evil-window-up
          :n "M-l"           #'evil-window-right
          :n "C-j"           #'move-text-down
          :n "C-k"           #'move-text-up
          :i "C-d"           #'delete-char
          :i "DEL"           #'org-delete-backward-char
          :n "gj"            #'evil-next-visual-line
          :n "gk"            #'evil-previous-visual-line
          :n "M-a"           #'+amos/mark-whole-buffer
          :n "M-q"           #'+amos/org-unfill-toggle
          :g "C-c e"         #'org-edit-special
          :g "C-c C-j"       #'counsel-org-goto
          :nv "C-S-f"        #'+amos/org-format ;; gui
          :nv "S-<f11>"      #'+amos/org-format ;; terminal
          :g "C-c C-S-l"     #'+org/remove-link))
  )

(after! org
  (setq-default org-M-RET-may-split-line '((default))
                org-adapt-indentation nil
                org-export-with-sub-superscripts nil
                org-agenda-dim-blocked-tasks nil
                org-agenda-files (concat +org-dir "/todo.org")
                org-agenda-inhibit-startup t
                org-agenda-skip-unavailable-files nil
                org-blank-before-new-entry '((heading . t) (plain-list-item . t))
                org-cycle-include-plain-lists t
                org-cycle-separator-lines 1
                org-default-notes-file (concat +org-dir "/note.org")
                org-ellipsis " ◢ "
                org-emphasis-alist '(("*" bold) ("/" italic) ("_" underline) ("=" org-verbatim verbatim) ("~" org-code verbatim) ("+" alert-urgent-face))
                org-entities-user '(("flat"  "\\flat" nil "" "" "266D" "♭") ("sharp" "\\sharp" nil "" "" "266F" "♯"))
                org-fontify-done-headline t
                org-fontify-quote-and-verse-blocks t
                org-fontify-whole-heading-line t
                org-footnote-auto-label 'plain
                org-hidden-keywords nil
                org-hide-emphasis-markers nil
                org-hide-leading-stars t
                org-hide-leading-stars-before-indent-mode t
                org-highlight-latex-and-related '(latex)
                org-image-actual-width '(200)
                org-image-actual-width nil
                org-indent-indentation-per-level 2
                org-indent-mode-turns-on-hiding-stars t
                org-latex-compiler "xelatex"
                org-latex-listings t
                org-startup-folded nil
                org-pretty-entities nil
                org-pretty-entities-include-sub-superscripts t
                org-priority-faces
                `((?a . ,(face-foreground 'error))
                  (?b . ,(face-foreground 'warning))
                  (?c . ,(face-foreground 'success)))
                org-refile-targets '((nil :maxlevel . 2))
                org-reverse-note-order t
                org-startup-indented t
                org-startup-truncated nil
                org-startup-with-inline-images nil
                org-tags-column 0
                org-todo-keywords '((sequence "TODO(T)" "|" "DONE(D)"))
                org-use-sub-superscripts '{}
                outline-blank-line t

                ;; LaTeX previews are too small and usually render to light backgrounds, so
                ;; this enlargens them and ensures their background (and foreground) match the
                ;; current theme.
                org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
                org-format-latex-options
                (plist-put org-format-latex-options
                           :background (face-attribute (or (cadr (assq 'default face-remapping-alist))
                                                           'default)
                                                       :background nil t)))

  ;; Use ivy/helm if either is available
  (when (or (modulep! :completion ivy)
            (modulep! :completion helm))
    (setq-default org-completion-use-ido nil
                  org-outline-path-complete-in-steps nil))

  (setq org-file-apps
        `(("\\.org$" . emacs)
          ("\\.cpp$" . emacs)
          ("\\.odt$" . "winopen %s")
          (system . default)
          (t . ,(cond (IS-MAC "open -R %s")
                      (IS-LINUX "xdg-open \"%s\""))))))
(after! ol
  (cl-pushnew '(file . find-file) org-link-frame-setup))

(defun +amos-org-remove-occur-highlights-h ()
  "Remove org occur highlights on ESC in normal mode."
  (when (and (derived-mode-p 'org-mode)
             org-occur-highlights)
    (org-remove-occur-highlights)))
(add-hook 'doom-escape-hook #'+amos-org-remove-occur-highlights-h)

(defun +amos-org-export-clear-single-linebreak-in-cjk-string-h (string &rest _)
  "clear single line-break between cjk characters that is usually soft line-breaks"
  (let* ((regexp "\\([\u4E00-\u9FA5]\\)\n\\([\u4E00-\u9FA5]\\)")
         (start (string-match regexp string)))
    (while start
      (setq string (replace-match "\\1\\2" nil nil string)
            start (string-match regexp string start))))
  string)
(after! ox
  (add-to-list 'org-export-filter-final-output-functions
               '+amos-org-export-clear-single-linebreak-in-cjk-string-h))


;; (defun +amos-org-export-output-file-name-a (args)
;;   "Return a centralized export location."
;;   (unless (nth 2 args)
;;     (setq args (append args (list org-export-directory))))
;;   args)
;; (advice-add #'org-export-output-file-name :filter-args #'+amos-org-export-output-file-name-a)

;; remove comments from org document for use with export hook
;; https://emacs.stackexchange.com/questions/22574/orgmode-export-how-to-prevent-a-new-line-for-comment-lines
(defun +amos-org-export-delete-org-comments-h (_)
  (cl-loop for comment in (reverse (org-element-map (org-element-parse-buffer)
                                       'comment 'identity))
           do (cl--set-buffer-substring (org-element-property :begin comment) (org-element-property :end comment) "")))
(add-hook! 'org-export-before-processing-hook #'+amos-org-export-delete-org-comments-h)

(use-package! ox-twbs
  :after ox)

(use-package! ox-hugo
  :after ox)

(use-package! org-projectile
  :after org
  :config
  (org-projectile-per-project)
  (setq org-projectile-per-project-filepath "TODO.org")
  (setq org-agenda-files (append org-agenda-files (org-projectile-todo-files))))

(defun +amos/projectile-todo ()
  (interactive)
  (require 'org)
  (let ((project
         (if (projectile-project-p)
             (projectile-project-name)
           (projectile-completing-read
            "Select which project's TODOs you would like to go to:"
            (occ-get-categories org-projectile-strategy)))))
    (let ((marker (occ-get-capture-marker (make-instance 'occ-context
                                                         :category project
                                                         :template org-projectile-capture-template
                                                         :strategy org-projectile-strategy
                                                         :options nil))))
      (switch-to-buffer (marker-buffer marker)))))

(defun +org*mu4e-mime-switch-headers-or-body ()
  "Switch the buffer to either mu4e-compose-mode (when in headers)
or org-mode (when in the body)."
  (interactive)
  (let* ((sepapoint
          (save-excursion
            (goto-char (point-min))
            (search-forward-regexp mail-header-separator nil t))))
    ;; only do stuff when the sepapoint exist; note that after sending the
    ;; message, this function maybe called on a message with the sepapoint
    ;; stripped. This is why we don't use `message-point-in-header'.
    (when sepapoint
      (cond
       ;; we're in the body, but in mu4e-compose-mode?
       ;; if so, switch to org-mode
       ((and (> (point) sepapoint) (eq major-mode 'mu4e-compose-mode))
        (org-mode)
        (add-hook 'before-save-hook
                  (lambda ()
                    (mu4e-error "Switch to mu4e-compose-mode (M-m) before saving."))
                  nil t)
        (org~mu4e-mime-decorate-headers)
        (local-set-key (kbd "M-m")
                       (lambda (keyseq)
                         (interactive "kEnter mu4e-compose-mode key sequence: ")
                         (let ((func (lookup-key mu4e-compose-mode-map keyseq)))
                           (if func (funcall func) (insert keyseq))))))
       ;; we're in the headers, but in org-mode?
       ;; if so, switch to mu4e-compose-mode
       ((and (<= (point) sepapoint) (eq major-mode 'org-mode))
        (org~mu4e-mime-undecorate-headers)
        (mu4e-compose-mode)
        ;; change to insert mode
        (evil-change-to-previous-state)
        (add-hook 'message-send-hook 'org~mu4e-mime-convert-to-html-maybe nil t)))
      ;; and add the hook
      (add-hook 'post-command-hook 'org~mu4e-mime-switch-headers-or-body t t))))
(advice-add #'org~mu4e-mime-switch-headers-or-body :override #'+org*mu4e-mime-switch-headers-or-body)

(defadvice org-open-file (around +org*org-open-file activate)
  (doom-with-advice (start-process-shell-command (lambda (orig_func cmd &rest _) (shell-command cmd)))
      ad-do-it))

(defun org-autolist-beginning-of-item-after-bullet ()
  "Returns the position before the first character after the
bullet of the current list item.

This function uses the same logic as `org-beginning-of-line' when
`org-special-ctrl-a/e' is enabled"
  (save-excursion
    (beginning-of-line 1)
    (when (looking-at org-list-full-item-re)
      (let ((box (match-end 3)))
        (if (not box) (match-end 1)
          (let ((after (char-after box)))
            (if (and after (= after ? )) (1+ box) box)))))))

(defun org-autolist-at-empty-item-description-p ()
  "Is point at an *empty* description list item?"
  (org-list-at-regexp-after-bullet-p "[ \t\r\n\v\f]"))

(defun +amos-org-return-a (ofun &rest args)
  "Wraps the org-return function to allow the Return key to
automatically insert new list items."
  (if (and (org-at-item-p)
           (eolp)
           (not
            (and org-return-follows-link
                 (eq 'org-link (get-text-property (point) 'face)))))
      (org-meta-return)
    (apply ofun args)))
(advice-add #'org-return :around #'+amos-org-return-a)

(defun +amos-org-delete-backward-char-a (ofun &rest args)
  "Wraps the org-delete-backward-char function to allow the Backspace
key to automatically delete list prefixes."
  (if (and (eq major-mode 'org-mode)
           (org-at-item-p)
           (org-autolist-at-empty-item-description-p))
      (delete-region (line-end-position)
                     (save-excursion (back-to-indentation)
                                     (point)))
    (apply ofun args)))
(advice-add #'org-delete-backward-char :around #'+amos-org-delete-backward-char-a)
(advice-add #'+amos/backward-delete-word :around #'+amos-org-delete-backward-char-a)

(defun +amos/list-todo ()
  (interactive)
  (require 'org)
  (setq org-agenda-files
        (-distinct (-non-nil
                    (delq nil (mapcar (lambda (file) (if (and (stringp file) (file-exists-p file)) file)) org-agenda-files)))))
  (org-todo-list))

(defun +amos/insert-todo-header ()
  (interactive)
  (evil-insert nil)
  (goto-char (point-max))
  (org-insert-heading '(16) t t))

(defun +amos/insert-header ()
  (interactive)
  (evil-insert nil)
  (end-of-visual-line)
  (let ((suffix (if (org-get-todo-state) "TODO " "")))
    (org-back-to-heading)
    (org-insert-heading)
    (insert suffix)))

(defun +amos/org-meta-return (&optional arg)
  (interactive "P")
  (org-check-before-invisible-edit 'insert)
  (evil-insert nil)
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively (cond (arg #'org-insert-heading)
                                ((org-at-table-p) #'org-table-wrap-region)
                                ((org-in-item-p) (cmd! (org-beginning-of-item)
                                                          (end-of-line)
                                                          (if (org-at-item-checkbox-p)
                                                              (org-insert-item 'checkbox)
                                                            (org-insert-item))))
                                ((org-get-todo-state) #'org-insert-todo-heading)
                                (t #'org-insert-heading))))
  (save-excursion
    (forward-line)
    (beginning-of-line)
    (unless (looking-at "[[:space:]]*$")
      (forward-line -1)
      (end-of-line)
      (newline))))

(advice-add #'org-previous-visible-heading :before #'leap-set-jump)
(advice-add #'outline-up-heading :before #'leap-set-jump)

(after! recentf
  ;; Don't clobber recentf with agenda files
  (defun +org-is-agenda-file (filename)
    (cl-find (file-truename filename) org-agenda-files
             :key #'file-truename
             :test #'equal))
  (push #'+org-is-agenda-file recentf-exclude))

(defun +amos/org-todo ()
  (interactive)
  (cond
   ((org-at-item-checkbox-p) (org-toggle-checkbox))
   ((org-at-item-p) (org-toggle-checkbox))
   (t (org-todo))))

(defun +amos/org-format ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (let ((last-line-is-empty t))
      (while (not (eobp))
        (let ((line-is-empty (looking-at-p "[ \t]*$"))
              (line-is-header (org-at-heading-or-item-p)))
          (if (and line-is-header (not last-line-is-empty))
               (save-excursion (end-of-line 0) (open-line 1)))
          (if (and last-line-is-empty line-is-empty)
              (delete-region
               (point)
               (progn (forward-visible-line 1) (point)))
            (forward-line 1)
            (setq last-line-is-empty line-is-empty)))))))

(defun +amos/ivy-bibtex (&optional arg)
  (interactive "P")
  (require 'ivy-bibtex)
  (when arg
    (bibtex-completion-clear-cache))
  (bibtex-completion-init)
  (let* ((candidates (bibtex-completion-candidates))
         (key (bibtex-completion-key-at-point))
         (preselect (and key
                         (cl-position-if (lambda (cand)
                                           (member (cons "=key=" key)
                                                   (cdr cand)))
                                         candidates))))
    (ivy-read "BibTeX entries: "
              candidates
              :preselect preselect
              :caller 'ivy-bibtex
              :action ivy-bibtex-default-action)))

(defun +amos/org-unfill-toggle ()
  "Toggle filling/unfilling of the current region, or current paragraph if no region active."
  (interactive)
  (let (deactivate-mark
        (fill-column
         (if (eq last-command this-command)
             (progn (setq this-command nil)
                    most-positive-fixnum)
           fill-column)))
    (call-interactively 'org-fill-paragraph)))

(after! ox-latex

  ;; update the list of LaTeX classes and associated header (encoding, etc.)
  ;; and structure
  '(add-to-list 'org-latex-classes
                `("beamer"
                  ,(concat "\\documentclass[presentation]{beamer}\n"
                           "[DEFAULT-PACKAGES]"
                           "[PACKAGES]"
                           "[EXTRA]\n")
                  ("\\section{%s}" . "\\section*{%s}")
                  ("\\subsection{%s}" . "\\subsection*{%s}")
                  ("\\subsubsection{%s}" . "\\subsubsection*{%s}"))))
