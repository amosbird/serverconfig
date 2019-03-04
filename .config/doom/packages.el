;; -*- no-byte-compile: t; -*-

(disable-packages! centered-window-mode
                   company-prescient
                   evil-escape
                   evil-matchit
                   evil-numbers
                   git-gutter-fringe
                   magithub
                   magit-todos
                   smerge-mode
                   dired-k
                   solaire-mode
                   stripe-buffer
                   vi-tilde-fringe
                   visual-fill-column
                   emacs-snippets)

(package! adoc-mode)
(package! ascii-art-to-unicode)
(package! bind-map)
(package! chinese-yasdcv)
(package! company-lsp)
(package! counsel-dash)
(package! dired-quick-sort)
(package! easy-hugo)
(package! evil-magit)
(package! fcitx)
(package! flycheck)
(package! flycheck-inline)
(package! general)
(package! git-gutter)
(package! git-timemachine)
(package! gitattributes-mode)
(package! gitconfig-mode)
(package! gitignore-mode)
(package! go-playground)
(package! htmlize)
(package! ivy-prescient)
(package! kurecolor)
(package! link-hint)
(package! lispyville)
(package! lsp-mode)
(package! lsp-ui)
(package! magit-svn)
(package! move-text)
(package! narrow-reindent)
(package! ov)
(package! page-break-lines)
(package! pdf-tools)
(package! quick-peek)
(package! rust-playground)
(package! smart-forward)
(package! speed-type)
(package! symbol-overlay)
(package! syntactic-close)
(package! try)
(package! unfill)
(package! ws-butler)
(package! yapfify)

(package! cc-playground :recipe (:fetcher github :repo "amosbird/cc-playground" :files ("*.el" "templates")))
(package! py-playground :recipe (:fetcher github :repo "amosbird/py-playground" :files ("*.el" "templates")))
(package! color-moccur :recipe (:fetcher github :repo "myuhe/color-moccur.el"))
(package! moccur-edit :recipe (:fetcher github :repo "myuhe/moccur-edit.el"))
(package! dired-hacks :recipe (:fetcher github :repo "Fuco1/dired-hacks"))
(package! direnv :recipe (:fetcher github :repo "wbolster/emacs-direnv"))
(package! evil-terminal-cursor-changer :recipe (:fetcher github :repo "amosbird/evil-terminal-cursor-changer"))
(package! evil-textobj-line :recipe (:fetcher github :repo "syohex/evil-textobj-line"))
(package! flyspell-lazy :recipe (:fetcher github :repo "rolandwalker/flyspell-lazy"))
(package! font-lock+ :recipe (:fetcher github :repo "emacsmirror/font-lock-plus"))
(package! stickyfunc-enhance :recipe (:fetcher github :repo "tuhdo/semantic-stickyfunc-enhance"))
(package! google-translate :recipe (:fetcher github :repo "atykhonov/google-translate"))
(package! help-fns+ :recipe (:fetcher github :repo "emacsmirror/help-fns-plus"))
(package! hl-line+ :recipe (:fetcher github :repo "emacsmirror/hl-line-plus"))
(package! ivy-rich :recipe (:fetcher github :repo "Yevgnen/ivy-rich"))
(package! osc :recipe (:fetcher github :repo "amosbird/osc.el"))
(package! rainbow-mode :recipe (:fetcher github :repo "amosbird/rainbow-mode"))
(package! realign-mode :recipe (:fetcher github :repo "amosbird/realign-mode.el"))
