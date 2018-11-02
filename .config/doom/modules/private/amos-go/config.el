;;; lang/go/config.el -*- lexical-binding: t; -*-

;;
;; Packages


(defun lsgo|enable ()
  (direnv-update-environment)
  (condition-case nil
      (lsp-go-enable)
    (user-error nil))
  (setq-local flycheck-checker 'lsp-ui)
  (lsp-ui-flycheck-add-mode major-mode)
  (add-to-list 'flycheck-checkers 'lsp-ui)
  (setq lsp-ui-flycheck-live-reporting nil))

(def-package! lsp-go
  :after go-mode
  :config
  (set-env! "GOPATH" "GOROOT")
  (set-docsets! 'go-mode "Go")
  (set-company-backend! 'go-mode 'company-lsp)
  (set-lookup-handlers! 'go-mode
    :definition #'+amos/definitions
    :references #'+amos/references
    :documentation #'counsel-dash-at-point)
  (add-hook 'go-mode-hook #'lsgo|enable)
  (map! :map go-mode-map
        :n "gd" #'+lookup/definition
        "C-c i" #'go-import-add))
