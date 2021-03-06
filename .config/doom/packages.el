;; -*- no-byte-compile: t; -*-

(disable-packages! centered-window-mode
                   company-prescient
                   doom-snippets
                   evil-escape
                   ;; evil-matchit
                   evil-numbers
                   git-gutter-fringe
                   magithub
                   magit-todos
                   smerge-mode
                   dired-k
                   forge
                   solaire-mode
                   stripe-buffer
                   vi-tilde-fringe
                   emacs-snippets)

(package! adoc-mode)
(package! bind-map)
(package! centered-cursor-mode)
(package! chinese-yasdcv)
(package! dired-quick-sort)
(package! easy-hugo)
(package! fcitx)
(package! flycheck-inline)
(package! git-link)
(package! git-gutter)
(package! gitattributes-mode)
(package! go-playground)
(package! magit-svn)
(package! move-text)
(package! narrow-reindent)
(package! ov)
(package! page-break-lines)
(package! quick-peek)
(package! rust-playground)
(package! speed-type)
(package! syntactic-close)
(package! try)
(package! unfill)
(package! visual-fill-column)
(package! yapfify)
(package! thrift-mode)

(package! rmsbolt :recipe (:host gitlab :repo "jgkamat/rmsbolt"))
(package! cc-playground :recipe (:host github :repo "amosbird/cc-playground" :files ("*.el" "templates")))
(package! py-playground :recipe (:host github :repo "amosbird/py-playground" :files ("*.el" "templates")))
(package! dired-hacks :recipe (:host github :repo "Fuco1/dired-hacks"))
(package! direnv :recipe (:host github :repo "wbolster/emacs-direnv"))
(package! evil-terminal-cursor-changer :recipe (:host github :repo "amosbird/evil-terminal-cursor-changer"))
(package! evil-textobj-line :recipe (:host github :repo "syohex/evil-textobj-line"))
(package! flyspell-lazy :recipe (:host github :repo "rolandwalker/flyspell-lazy"))
(package! font-lock+ :recipe (:host github :repo "emacsmirror/font-lock-plus"))
(package! stickyfunc-enhance :recipe (:host github :repo "tuhdo/semantic-stickyfunc-enhance"))
(package! google-translate :recipe (:host github :repo "atykhonov/google-translate"))
(package! help-fns+ :recipe (:host github :repo "emacsmirror/help-fns-plus"))
(package! hl-line+ :recipe (:host github :repo "emacsmirror/hl-line-plus"))
(package! sync-recentf :recipe (:host github :repo "ffevotte/sync-recentf"))
(package! osc :recipe (:host github :repo "amosbird/osc.el"))
(package! rainbow-mode :recipe (:host github :repo "amosbird/rainbow-mode"))
(package! realign-mode :recipe (:host github :repo "amosbird/realign-mode.el"))
