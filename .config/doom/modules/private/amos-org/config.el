;;; org/org/config.el -*- lexical-binding: t; -*-

;; Custom variables
(defvar +org-dir (expand-file-name "~/org/")
  "The directory where org files are kept.")
(defvaralias 'org-directory '+org-dir)

(add-hook 'org-load-hook #'+org|init)
(add-hook 'org-mode-hook #'+org|hook)

;;
;; Plugins
;;

(def-package! toc-org
  :commands toc-org-enable
  :init (add-hook 'org-mode-hook #'toc-org-enable))

(def-package! org-crypt ; built-in
  :commands org-crypt-use-before-save-magic
  :init (add-hook 'org-load-hook #'org-crypt-use-before-save-magic)
  :config
  (setq org-tags-exclude-from-inheritance '("crypt")
        org-crypt-key user-mail-address
        epa-file-encrypt-to user-mail-address))

(def-package! org-bullets
  :commands org-bullets-mode
  :init (add-hook 'org-mode-hook #'org-bullets-mode))

(def-package! evil-org
  :after org
  :config
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
   :n "RET"           #'org-open-at-point
   :n "M-h"           #'evil-window-left
   :n "M-j"           #'evil-window-down
   :n "M-k"           #'evil-window-up
   :n "M-l"           #'evil-window-right
   :n "C-j"           #'org-metadown
   :n "C-k"           #'org-metaup
   :i "C-d"           #'delete-char
   :i "DEL"           #'org-delete-backward-char
   :n  "gj"           #'evil-next-visual-line
   :n  "gk"           #'evil-previous-visual-line
   :n "M-a"           #'+amos/mark-whole-buffer
   :g "C-c e"         #'org-edit-special
   :g "C-c C-j"       #'counsel-org-goto
   :g "C-c C-S-l"     #'+org/remove-link)))

;;
;; Hooks & bootstraps
;;

(defun +org|hook ()
  (setq line-spacing 1)
  (visual-line-mode +1)
  (org-indent-mode -1)
  (doom|disable-line-numbers)

  ;; show-paren-mode causes problems for org-indent-mode, so disable it
  (set (make-local-variable 'show-paren-mode) nil)

  (unless org-agenda-inhibit-startup
    ;; My version of the 'overview' #+STARTUP option: expand first-level
    ;; headings. Expands the first level, but no further.
    (when (eq org-startup-folded t)
      (outline-hide-sublevels 2))

    ;; If saveplace places the point in a folded position, unfold it on load
    (when (outline-invisible-p)
      (ignore-errors
        (save-excursion
          (outline-previous-visible-heading 1)
          (org-show-subtree))))))

(defun +org|init ()
  "Run once, when org is first loaded."
  (+org-init-ui)
  (+org-hacks))

;;
(defun +org-init-ui ()
  "Configures the UI for `org-mode'."
  (setq-default
   org-M-RET-may-split-line '((default))
   org-adapt-indentation nil
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
   org-startup-folded nil
   org-pretty-entities nil
   org-pretty-entities-include-sub-superscripts t
   org-priority-faces
   `((?a . ,(face-foreground 'error))
     (?b . ,(face-foreground 'warning))
     (?c . ,(face-foreground 'success)))
   org-refile-targets '((nil :level . 1))
   org-reverse-note-order t
   org-startup-indented t
   org-startup-with-inline-images nil
   org-tags-column 0
   ;; org-todo-keywords '((sequence "[ ](t)" "[-](p)" "[?](m)" "|" "[X](d)")
   ;;                     (sequence "TODO(T)" "|" "DONE(D)")
   ;;                     (sequence "IDEA(i)" "NEXT(n)" "ACTIVE(a)" "WAITING(w)" "LATER(l)" "|" "CANCELLED(c)"))
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
  (when (or (featurep! :completion ivy)
            (featurep! :completion helm))
    (setq-default org-completion-use-ido nil
                  org-outline-path-complete-in-steps nil)))

;;
(defun +org-hacks ()
  "Getting org to behave."
  ;; Don't open separate windows
  (cl-pushnew '(file . find-file) org-link-frame-setup)

  ;; Let OS decide what to do with files when opened
  (setq org-file-apps
        `(("\\.org$" . emacs)
          ("\\.cpp$" . emacs)
          ("\\.odt$" . "winopen %s")
          (system . default)
          (t . ,(cond (IS-MAC "open -R %s")
                      (IS-LINUX "xdg-open \"%s\"")))))

  (defun +org|remove-occur-highlights ()
    "Remove org occur highlights on ESC in normal mode."
    (when (and (derived-mode-p 'org-mode)
               org-occur-highlights)
      (org-remove-occur-highlights)))
  (add-hook '+evil-esc-hook #'+org|remove-occur-highlights))

(add-hook 'org-load-hook #'+org-export|init t)

;(def-package! ox-pandoc
;  :config
;  (unless (executable-find "pandoc")
;    (warn "org-export: couldn't find pandoc, disabling pandoc export"))
;  (setq org-pandoc-options
;        '((standalone . t)
;          (mathjax . t)
;          (parse-raw . t))))

(defun +org-export|init ()
  (setq org-export-directory (expand-file-name "export" +org-dir)
        org-export-backends '(ascii html latex md beamer odt)
        org-export-with-toc t
        org-export-with-author t)

  ;; Always export to a central location
  (unless (file-directory-p org-export-directory)
    (make-directory org-export-directory t))
  (defun +org*export-output-file-name (args)
    "Return a centralized export location."
    (unless (nth 2 args)
      (setq args (append args (list org-export-directory))))
    args)
  (advice-add #'org-export-output-file-name
              :filter-args #'+org*export-output-file-name)

  (defun +org-export|clear-single-linebreak-in-cjk-string (string &optional backend info)
    "clear single line-break between cjk characters that is usually soft line-breaks"
    (let* ((regexp "\\([\u4E00-\u9FA5]\\)\n\\([\u4E00-\u9FA5]\\)")
           (start (string-match regexp string)))
      (while start
        (setq string (replace-match "\\1\\2" nil nil string)
              start (string-match regexp string start))))
    string)

  ;; remove comments from org document for use with export hook
  ;; https://emacs.stackexchange.com/questions/22574/orgmode-export-how-to-prevent-a-new-line-for-comment-lines
  (defun +org-export|delete-org-comments (backend)
    (loop for comment in (reverse (org-element-map (org-element-parse-buffer)
                                      'comment 'identity))
          do
          (setf (buffer-substring (org-element-property :begin comment)
                                  (org-element-property :end comment)) "")))
  (add-hook! 'org-export-before-processing-hook #'+org-export|delete-org-comments))


(def-package! ox-twbs
  :after ox)

(def-package! ox-hugo
  :after ox)

(def-package! org-projectile
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

(after! ox
  (add-to-list 'org-export-filter-final-output-functions
               '+org-export|clear-single-linebreak-in-cjk-string))

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
  (message "evaluating...")
  (org-list-at-regexp-after-bullet-p "\\(\\s-*\\)::\\(\\s-*$\\)"))

(defadvice org-return (around org-autolist-return)
  "Wraps the org-return function to allow the Return key to
automatically insert new list items."
  (if (and (org-at-item-p)
           (not
            (and org-return-follows-link
                 (eq 'org-link (get-text-property (point) 'face)))))
      (if (and (eolp)
               (<= (point) (org-autolist-beginning-of-item-after-bullet)))
          (condition-case nil
              (call-interactively 'org-outdent-item)
            ('error (delete-region (line-beginning-position)
                                   (line-end-position))))
        (cond
         ((org-at-item-checkbox-p)
          (org-insert-todo-heading nil))
         ((and (org-at-item-description-p)
               (> (point) (org-autolist-beginning-of-item-after-bullet))
               (< (point) (line-end-position)))
          (newline))
         (t
          (org-meta-return))))
    ad-do-it))

(defadvice org-delete-backward-char (around org-autolist-delete-backward-char)
  "Wraps the org-delete-backward-char function to allow the Backspace
key to automatically delete list prefixes."
  (if (and (org-at-item-p)
           (<= (point) (org-autolist-beginning-of-item-after-bullet)))
      (progn
          (goto-char (org-autolist-beginning-of-item-after-bullet))
          (cond
           ((= 1 (line-number-at-pos))
            (delete-region (point) (line-beginning-position)))
           ((org-autolist-at-empty-item-description-p)
            (delete-region (line-end-position)
                           (save-excursion (forward-line -2)
                                           (line-end-position))))
           (t
            (delete-region (point)
                           (save-excursion (forward-line -2)
                                           (line-end-position))))))
    ad-do-it))

;;;###autoload
(define-minor-mode org-autolist-mode
  "Enables improved list management in org-mode."
  nil " Autolist" nil
  (cond
   ;; If enabling org-autolist-mode, then add our advice functions.
   (org-autolist-mode
    (ad-activate 'org-return)
    (ad-activate 'org-delete-backward-char))
   ;; Be sure to clean up after ourselves when org-autolist-mode gets disabled.
   (t
    (ad-deactivate 'org-return)
    (ad-deactivate 'org-delete-backward-char))))

(add-hook! org-mode (org-autolist-mode))

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
    (org-insert-heading-after-current)
    (insert suffix)))

(defun +amos/org-meta-return (&optional arg)
  (interactive "P")
  (org-check-before-invisible-edit 'insert)
  (evil-insert nil)
  (or (run-hook-with-args-until-success 'org-metareturn-hook)
      (call-interactively (cond (arg #'org-insert-heading)
                                ((org-at-table-p) #'org-table-wrap-region)
                                ((org-in-item-p) (lambda! (org-beginning-of-item)
                                                     (end-of-line)
                                                     (if (org-at-item-checkbox-p)
                                                         (org-insert-item 'checkbox)
                                                       (org-insert-item))))
                                ((org-get-todo-state) #'org-insert-todo-heading)
                                (t #'org-insert-heading))))
  (save-excursion
    (next-line)
    (beginning-of-line)
    (unless (looking-at "[[:space:]]*$")
      (previous-line)
      (end-of-line)
      (newline))))

(advice-add #'org-previous-visible-heading :before #'evil-set-jump)
(advice-add #'outline-up-heading :before #'evil-set-jump)

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
   ((org-at-item-checkbox-p) (+org/toggle-checkbox))
   ((org-at-item-p) (+org/toggle-checkbox))
   (t (org-todo))))
