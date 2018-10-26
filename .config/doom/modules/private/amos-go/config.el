;;; lang/go/config.el -*- lexical-binding: t; -*-

;;
;; Packages

(after! go-mode
  (set-env! "GOPATH" "GOROOT")
  (set-docsets! 'go-mode "Go")
  (set-company-backend! 'go-mode 'company-lsp)
  (set-lookup-handlers! 'go-mode
    :definition #'+lookup/definition
    :references #'+lookup/references
    :documentation #'counsel-dash-at-point)
  (add-hook 'go-mode-hook #'lsp-go-enable)
  (map! :map go-mode-map
        "C-c i" #'go-import-add))
