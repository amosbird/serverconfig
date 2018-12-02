;;; lang/java/config.el -*- lexical-binding: t; -*-

;;
;; Packages

(def-package! lsp-java
  :ensure t
  :config
  (set-company-backend! 'java-mode 'company-lsp)
  (add-hook 'java-mode-hook 'lsp-java-enable)
  (add-hook 'java-mode-hook 'flycheck-mode)
  (add-hook 'java-mode-hook 'company-mode)
  ;; (add-hook 'java-mode-hook 'lsp-ui-mode)

  (set-lookup-handlers! 'java-mode
    :definition #'+amos/definitions
    :references #'+amos/references
    :documentation #'counsel-dash-at-point)

  (map! :map java-mode-map
        :n "M-o"      #'lsp-ui-sideline-mode))
