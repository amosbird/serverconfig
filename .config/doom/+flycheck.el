(defun +amos/yank-flycheck-error ()
  (interactive)
  (catch 'return
    (let ((overlays (overlays-at (point))))
      (cl-loop for overlay in overlays
               do (when-let (str (overlay-get overlay 'flycheck-error))
                    (kill-new (flycheck-error-message str))
                    (throw 'return nil)))
      (cl-loop for overlay in overlays
               do (when-let (str (overlay-get overlay 'flycheck-warning))
                    (kill-new (flycheck-error-message str))
                    (throw 'return nil))))))

(after! flycheck (global-flycheck-inline-mode))
(setq flycheck-highlighting-mode 'columns
      flycheck-indication-mode nil)
        ;; flycheck-inline-display-function
        ;; (lambda (msg pos)
        ;;   (let* ((ov (quick-peek-overlay-ensure-at pos))
        ;;          (contents (quick-peek-overlay-contents ov)))
        ;;     (setf (quick-peek-overlay-contents ov)
        ;;           (concat contents (when contents "\n") msg))
        ;;     (quick-peek-update ov)))
        ;; flycheck-inline-clear-function #'quick-peek-hide

;; (defun +amos-set-evil-move-beyond-eol-nil-h (&rest _)
;;   (setq evil-move-beyond-eol nil)
;;   (evil-adjust-cursor)
;;   (advice-remove #'flycheck-perform-deferred-syntax-check #'+amos-set-evil-move-beyond-eol-nil-h))

;; (defun +amos/flycheck-next-error ()
;;   (interactive)
;;   (setq evil-move-beyond-eol t)
;;   (flycheck-next-error-function 1 nil)
;;   (recenter)
;;   (flycheck-display-error-at-point-soon)
;;   (advice-add #'flycheck-perform-deferred-syntax-check :after #'+amos-set-evil-move-beyond-eol-nil-h))

;; (defun +amos/flycheck-previous-error ()
;;   (interactive)
;;   (setq evil-move-beyond-eol t)
;;   (call-interactively #'flycheck-previous-error)
;;   (flycheck-next-error-function -1 nil)
;;   (recenter)
;;   (flycheck-display-error-at-point-soon)
;;   (advice-add #'flycheck-perform-deferred-syntax-check :after #'+amos-set-evil-move-beyond-eol-nil-h))

;; (defun +amos-flycheck-display-error-at-point-soon-a ()
;;   (when (flycheck-overlays-at (point))
;;     (with-demoted-errors "Flycheck error display error: %s"
;;       (when flycheck-mode
;;         (-when-let (errors (flycheck-overlay-errors-at (point)))
;;           (flycheck-display-errors errors))))))
;; (advice-add #'flycheck-display-error-at-point-soon :override #'+amos-flycheck-display-error-at-point-soon-a)


;; (defun +amos*flycheck-error-line-region (err)
;;   (flycheck-error-with-buffer err
;;     (save-restriction
;;       (save-excursion
;;         (widen)
;;         (goto-char (point-min))
;;         (forward-line (- (flycheck-error-line err) 1))
;;         (let ((end (line-end-position)))
;;           (skip-syntax-forward " " end)
;;           (backward-prefix-chars)
;;           (cons (point) (if (eolp) (+ 1 end) end)))))))

;; (defun +amos*flycheck-error-column-region (err)
;;   (flycheck-error-with-buffer err
;;     (save-restriction
;;       (save-excursion
;;         (-when-let (column (flycheck-error-column err))
;;           (widen)
;;           (goto-char (point-min))
;;           (forward-line (- (flycheck-error-line err) 1))
;;           (cond
;;            ((eobp)                    ; Line beyond EOF
;;             (cons (- (point-max) 1) (point-max)))
;;            ((eolp)                    ; Empty line
;;             nil)
;;            (t
;;             (let ((end (min (+ (point) column)
;;                             (+ (line-end-position) 1))))
;;               (cons (- end 1) end)))))))))

;; (advice-add #'flycheck-error-line-region :override #'+amos*flycheck-error-line-region)
;; (advice-add #'flycheck-error-column-region :override #'+amos*flycheck-error-column-region)

;; (defun +amos/yank-flycheck-error ()
;;   (interactive)
;;   (catch 'return
;;     (let ((overlays (overlays-at (point))))
;;       (cl-loop for overlay in overlays
;;                do (when-let (str (overlay-get overlay 'flycheck-error))
;;                     (kill-new (flycheck-error-message str))
;;                     (throw 'return nil)))
;;       (cl-loop for overlay in overlays
;;                do (when-let (str (overlay-get overlay 'flycheck-warning))
;;                     (kill-new (flycheck-error-message str))
;;                     (throw 'return nil))))))

;; (advice-add #'flycheck-previous-error :after (lambda (&rest _) (recenter)))
;; (advice-add #'flycheck-next-error :after (lambda (&rest _) (recenter)))

;; (defun +amos-flycheck-next-error-function-a (n reset)
;;   (-if-let* ((pos (flycheck-next-error-pos n reset))
;;              (err (get-char-property pos 'flycheck-error))
;;              (filename (flycheck-error-filename err))
;;              (dummy (string= buffer-file-name filename)))
;;       (progn
;;         (leap-set-jump)
;;         (flycheck-jump-to-error err))
;;     (user-error "No more Flycheck errors")))
;; (advice-add #'flycheck-next-error-function :override #'+amos-flycheck-next-error-function-a)

;; (defun +amos-flycheck-inline-display-errors-a (ofun &rest candidate)
;;   (if (or (memq this-command '(+amos/flycheck-previous-error
;;                                +amos/flycheck-next-error
;;                                flycheck-previous-error
;;                                flycheck-next-error
;;                                +amos/yank-flycheck-error))
;;           (eq last-input-event 29))
;;       (apply ofun candidate)))
;; (advice-add #'flycheck-inline-display-errors :around #'+amos-flycheck-inline-display-errors-a)

;; ;; unwind flycheck backtrace
;; ;; (defun doom*flycheck-buffer ()
;; ;;   (interactive)
;; ;;   (flycheck-clean-deferred-check)
;; ;;   (if flycheck-mode
;; ;;       (unless (flycheck-running-p)
;; ;;         (run-hooks 'flycheck-before-syntax-check-hook)
;; ;;         (flycheck-clear-errors)
;; ;;         (flycheck-mark-all-overlays-for-deletion)
;; ;;         (let* ((checker (flycheck-get-checker-for-buffer)))
;; ;;           (if checker
;; ;;               (flycheck-start-current-syntax-check checker)
;; ;;             (flycheck-clear)
;; ;;             (flycheck-report-status 'no-checker))))
;; ;;     (user-error "Flycheck mode disabled")))
;; ;; (advice-add #'flycheck-buffer :override #'doom*flycheck-buffer)
;; ;; (defun doom*flycheck-start-command-checker (checker callback)
;; ;;   (let (process)
;; ;;     (let* ((program (flycheck-find-checker-executable checker))
;; ;;            (args (flycheck-checker-substituted-arguments checker))
;; ;;            (command (funcall flycheck-command-wrapper-function
;; ;;                              (cons program args)))
;; ;;            (process-connection-type nil))
;; ;;       (setq process (apply 'start-process (format "flycheck-%s" checker)
;; ;;                            nil command))
;; ;;       (setf (process-sentinel process) #'flycheck-handle-signal)
;; ;;       (setf (process-filter process) #'flycheck-receive-checker-output)
;; ;;       (set-process-query-on-exit-flag process nil)
;; ;;       (process-put process 'flycheck-checker checker)
;; ;;       (process-put process 'flycheck-callback callback)
;; ;;       (process-put process 'flycheck-buffer (current-buffer))
;; ;;       (process-put process 'flycheck-working-directory default-directory)
;; ;;       (process-put process 'flycheck-temporaries flycheck-temporaries)
;; ;;       (setq flycheck-temporaries nil)
;; ;;       (when (flycheck-checker-get checker 'standard-input)
;; ;;         (flycheck-process-send-buffer process))
;; ;;       process)))
;; ;; (advice-add #'flycheck-start-command-checker :override #'doom*flycheck-start-command-checker)

;; (setq-hook! 'lua-mode-hook flycheck-highlighting-mode 'lines)
