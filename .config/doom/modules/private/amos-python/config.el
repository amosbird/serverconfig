;;; lang/python/config.el -*- lexical-binding: t; -*-

(defun pyls|enable ()
  (direnv-update-environment)
  (condition-case nil
      (lsp-python-enable)
    (user-error nil))
  (setq-local flycheck-checker 'lsp-ui)
  (lsp-ui-flycheck-add-mode major-mode)
  (add-to-list 'flycheck-checkers 'lsp-ui)
  (setq lsp-ui-flycheck-live-reporting nil))

(def-package! python
  :defer t
  :init
  (setq python-environment-directory doom-cache-dir
        python-indent-guess-indent-offset-verbose nil)
  :config
  (set-env! "PYTHONPATH" "PYENV_ROOT" "ANACONDA_HOME")
  (set-electric! 'python-mode :chars '(?:))
  (set-company-backend! 'python-mode 'company-lsp)
  (set-lookup-handlers! 'python-mode
    :definition #'+lookup/definition
    :references #'+lookup/references
    :documentation #'counsel-dash-at-point)

  (set-pretty-symbols! 'python-mode
                       ;; Functional
                       :def "def"
                       :lambda "lambda"
                       ;; Types
                       :null "None"
                       :true "True" :false "False"
                       :int "int" :str "str"
                       :float "float"
                       :bool "bool"
                       :tuple "tuple"
                       ;; Flow
                       :not "not"
                       :in "in" :not-in "not in"
                       :and "and" :or "or"
                       :for "for"
                       :return "return" :yield "yield")

  (define-key python-mode-map (kbd "DEL") nil) ; interferes with smartparens
  (sp-local-pair 'python-mode "'" nil
                 :unless '(sp-point-before-word-p
                           sp-point-after-word-p
                           sp-point-before-same-p))

  (setq-hook! 'python-mode-hook tab-width python-indent-offset)
  (require 'lsp-mode)
  (require 'lsp-python)
  (add-hook 'python-mode-hook #'pyls|enable))
