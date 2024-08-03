(setq lsp-enable-file-watchers nil)
(setq lsp-enable-imenu nil)
(setq lsp-eldoc-enable-hover nil)
(setq lsp-ui-imenu-enable nil)
(setq lsp-ui-doc-enable nil)
(setq lsp-enable-symbol-highlighting nil)
(setq lsp-enable-on-type-formatting nil)
(setq lsp-enable-dap-auto-configure nil)
(setq lsp-auto-guess-root t)
;; (setq lsp-headerline-breadcrumb-enable t)
(setq lsp-enable-indentation nil)
;; TODO remember to disable it, maybe notify on modeline
;; (setq lsp-log-io t)

(advice-add #'flymake-eldoc-function :override #'ignore)

(add-hook! prog-mode #'flymake-mode)
(add-hook! prog-mode #'flymake-popon-mode)
(add-hook! prog-mode #'treecrumbs-mode)
;; (add-hook! prog-mode #'breadcrumb-local-mode)
(after! lsp-mode
  (setq lsp-diagnostics-provider :flymake))

(global-eldoc-mode -1)
(setq eglot-stay-out-of '(eldoc))
(defun +amos/yank-flymake-error ()
  (interactive)
  (when-let ((diagnostic
              (mapconcat
               #'flymake-diagnostic-text
               (flymake-diagnostics (point))
               "\n")))
    (kill-new diagnostic)))

(advice-add #'lsp-ui-mode :override #'ignore)

(defun +amos*evil-normal-post-command ()
  (setq evil-move-beyond-eol nil)
  (advice-remove #'evil-normal-post-command #'+amos*evil-normal-post-command))

(defun +amos-flymake-goto-error (&optional p)
  (setq evil-move-beyond-eol t)
  (advice-add #'evil-normal-post-command :after #'+amos*evil-normal-post-command)
  (if p (flymake-goto-prev-error 1)
    (flymake-goto-next-error 1)))

(defun +amos/flymake-goto-prev-error ()
  (interactive)
  (+amos-flymake-goto-error t))

(defun +amos/flymake-goto-next-error ()
  (interactive)
  (+amos-flymake-goto-error))

(add-hook! 'evil-normal-state-entry-hook (flymake-popon-mode +1))
(add-hook! 'evil-normal-state-exit-hook (flymake-popon-mode -1))

(after! python
  (add-hook! python-ts-mode #'lsp))

(after! java-ts-mode
  (add-hook! java-ts-mode #'lsp))

(setq lsp-java-vmargs
      '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true" "-Xmx4G" "-Xms1G"))

;; (setq lsp-response-timeout 5)
;; (setq lsp-signature-render-all nil)

;; (use-package! lsp-mode
;;   :init
;;   (setq
;;    lsp-prefer-flymake nil
;;    lsp-log-io nil
;;    lsp-enable-indentation nil
;;    lsp-enable-file-watchers nil
;;    lsp-auto-guess-root t)
;;   :defer
;;   :config
;;   (add-hook! 'kill-emacs-hook (setq lsp-restart 'ignore))
;;   (add-hook! 'lsp-after-open-hook #'lsp-enable-imenu))

;; (defun lsp-java-suggest-project-root ()
;;   (and (memq major-mode '(java-mode))
;;        (when-let (dir (cl-some  #'(lambda (file) (locate-dominating-file default-directory file)) '("pom.xml")))
;;          (expand-file-name dir))))

;; (advice-add 'lsp--suggest-project-root :before-until #'lsp-java-suggest-project-root)

;; (use-package! lsp-ui
;;   :init
;;   (setq lsp-ui-sideline-enable nil
;;         lsp-ui-sideline-show-diagnostics nil)
;;   :defer)

;; (defun +amos/lsp-highlight-symbol ()
;;   (interactive)
;;   (let ((url (thing-at-point 'url)))
;;     (if url (goto-address-at-point)
;;       (let ((inhibit-message t))
;;         (setq-local +amos--lsp-maybe-highlight-symbol t)
;;         (lsp--document-highlight)))))

;; (defun +amos-lsp-remove-highlight-h ()
;;   (interactive)
;;   (when (and (boundp '+amos--lsp-maybe-highlight-symbol) +amos--lsp-maybe-highlight-symbol)
;;     (--each (lsp-workspaces)
;;       (with-lsp-workspace it
;;                           (lsp--remove-overlays 'lsp-highlight)))
;;     (setq-local +amos--lsp-maybe-highlight-symbol nil)))

;; (add-hook 'doom-escape-hook #'+amos-lsp-remove-highlight-h)

;; (cl-defun +amos-lsp-find-custom (kind method &optional extra &key display-action)
;;   "Send request named METHOD and get cross references of the symbol under point.
;; EXTRA is a plist of extra parameters."
;;   (let ((loc (lsp-request method
;;                           (append (lsp--text-document-position-params) extra))))
;;     (if loc
;;         (+amos-ivy-xref (lsp--locations-to-xref-items (if (sequencep loc) loc (list loc))) kind)
;;       (message "Not found for: %s" (thing-at-point 'symbol t)))))

;; (defun +amos/definitions ()
;;   (interactive)
;;   (+amos-lsp-find-custom 'definitions "textDocument/definition"))

;; (defun +amos/references ()
;;   (interactive)
;;   (+amos-lsp-find-custom 'references "textDocument/references"))

;; (defun +amos-lsp--position-to-point-a (params)
;;   "Convert Position object in PARAMS to a point."
;;   (save-excursion
;;     (save-restriction
;;       (widen)
;;       (goto-char (point-min))
;;       ;; We use `goto-char' to ensure that we return a point inside the buffer
;;       ;; to avoid out of range error
;;       (goto-char (+ (line-beginning-position (1+ (gethash "line" params)))
;;                     (gethash "character" params)))
;;       (point))))

;; (advice-add #'lsp--position-to-point :override #'+amos-lsp--position-to-point-a)
;; (advice-add #'lsp-ui-sideline--diagnostics-changed :override #'ignore)

;; (defun +amos/lsp-ui-imenu ()
;;   (interactive)
;;   (setq lsp-ui-imenu--origin (current-buffer))
;;   (imenu--make-index-alist)
;;   (let ((list imenu--index-alist))
;;     (with-current-buffer (get-buffer-create "*lsp-ui-imenu*")
;;       (let* ((padding (or (and (eq lsp-ui-imenu-kind-position 'top) 1)
;;                           (--> (-filter 'imenu--subalist-p list)
;;                             (--map (length (car it)) it)
;;                             (-max (or it '(1))))))
;;              (grouped-by-subs (-partition-by 'imenu--subalist-p list))
;;              (color-index 0)
;;              buffer-read-only)
;;         (remove-overlays)
;;         (erase-buffer)
;;         (lsp-ui-imenu--put-separator)
;;         (dolist (group grouped-by-subs)
;;           (if (imenu--subalist-p (car group))
;;               (dolist (kind group)
;;                 (-let* (((title . entries) kind))
;;                   (lsp-ui-imenu--put-kind title padding color-index)
;;                   (--each-indexed entries
;;                     (insert (lsp-ui-imenu--make-line title it-index padding it color-index)))
;;                   (lsp-ui-imenu--put-separator)
;;                   (setq color-index (1+ color-index))))
;;             (--each-indexed group
;;               (insert (lsp-ui-imenu--make-line " " it-index padding it color-index)))
;;             (lsp-ui-imenu--put-separator)
;;             (setq color-index (1+ color-index))))
;;         (lsp-ui-imenu-mode)
;;         (setq mode-line-format '(:eval (lsp-ui-imenu--win-separator)))
;;         (goto-char 1)
;;         (add-hook 'post-command-hook 'lsp-ui-imenu--post-command nil t)))
;;     (let ((win (display-buffer-in-side-window (get-buffer "*lsp-ui-imenu*") '((side . right))))
;;           (fit-window-to-buffer-horizontally t))
;;       (set-window-margins win 1)
;;       (select-window win)
;;       (set-window-start win 1)
;;       (set-window-dedicated-p win t)
;;       (let ((fit-window-to-buffer-horizontally 'only))
;;         (fit-window-to-buffer win))
;;       (window-resize win 20 t))))

;; (use-package! lsp-python-ms
;;   :hook (python-mode . lsp)
;;   :config
;;   ;; for dev build of language server
;;   (setq lsp-python-ms-dir
;;         (expand-file-name "~/git/python-language-server/output/bin/release/"))
;;   ;; for executable of language server, if it's not symlinked on your PATH
;;   (setq lsp-python-ms-executable
;;         "~/git/python-language-server/output/bin/release/Microsoft.Python.LanguageServer"))
