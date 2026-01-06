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
                   kkp
                   helpful
                   emacs-snippets)

;; (unpin! format-all)

(package! adoc-mode)
(package! bind-map)
(package! centered-cursor-mode)
(package! chinese-yasdcv)
(package! dired-quick-sort)
(package! easy-hugo)
(package! fcitx)
(package! git-link)
(package! git-gutter)
(package! gitattributes-mode)
;; (package! go-playground)
(package! magit-svn)
(package! magit-section)
(package! move-text)
(package! narrow-reindent)
(package! ov)
;; (package! page-break-lines)
(package! quick-peek)
;; (package! rust-playground)
(package! speed-type)
(package! syntactic-close)
(package! try)
(package! unfill)
(package! visual-fill-column)
(package! yapfify)
(package! thrift-mode)
(package! deadgrep)

(package! avy)

(package! breadcrumb)

;; (package! treemacs-nerd-icons :recipe (:host github :repo "rainstormstudio/treemacs-nerd-icons"))
(package! treesit-auto :recipe (:host github :repo "renzmann/treesit-auto"))
(package! rmsbolt :recipe (:host gitlab :repo "jgkamat/rmsbolt"))
;; (package! cc-playground :recipe (:host github :repo "amosbird/cc-playground" :files ("*.el" "templates")))
;; (package! py-playground :recipe (:host github :repo "amosbird/py-playground" :files ("*.el" "templates")))
(package! dired-hacks :recipe (:host github :repo "Fuco1/dired-hacks"))
;; (package! direnv :recipe (:host github :repo "amosbird/emacs-direnv"))
;; (package! direnv :recipe (:host github :repo "wbolster/emacs-direnv"))
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
;; (package! rainbow-mode :recipe (:host github :repo "amosbird/rainbow-mode"))
(package! realign-mode :recipe (:host github :repo "amosbird/realign-mode.el"))
(package! flymake-popon :recipe (:type git :repo "https://codeberg.org/akib/emacs-flymake-popon.git"))
(package! kkp :recipe (:host github :repo "benjaminor/kkp"))
(package! aider :recipe (:host github :repo "tninja/aider.el" :files ("aider.el" "aider-doom.el")))
(package! gptel)
(package! gptel-quick :recipe (:host github :repo "karthink/gptel-quick"))
(package! evedel :recipe (:host github :repo "daedsidog/evedel"))
(package! protobuf-mode)

(package! gitconfig-mode
   :recipe (:host github :repo "magit/git-modes"
   :files ("gitconfig-mode.el")))
(package! gitignore-mode
   :recipe (:host github :repo "magit/git-modes"
   :files ("gitignore-mode.el")))
(package! gitattributes-mode
   :recipe (:host github :repo "magit/git-modes"
   :files ("gitattributes-mode.el")))

(package! embark :pin "86fc40909ba9506676a28d35879f8c091018ac42" :recipe (:host github :repo "amosbird/embark"))
(package! embark-consult :pin "86fc40909ba9506676a28d35879f8c091018ac42" :recipe (:host github :repo "amosbird/embark"))

(package! magit-prime :recipe (:host github :repo "Azkae/magit-prime"))

;; (package! vertico :pin "edbb370ad237781eb893a5c2b505cfa24e6b2d9a")
;; (package! consult :pin "fa249d5dd7212e5ae1fa51c086d8f1197d738ef4")

;; This is temporarily necessary due to an unrelated bug.
(unpin! gitignore-mode gitconfig-mode gitattributes-mode evil-terminal-cursor-changer)

(unpin! corfu cape)
(disable-packages! corfu-terminal)

;; (when (package! lsp-bridge
;;         :recipe (:host github
;;                  :repo "manateelazycat/lsp-bridge"
;;                  :branch "master"
;;                  :files ("*.el" "*.py" "acm" "core" "langserver" "multiserver" "resources")
;;                  ;; do not perform byte compilation or native compilation for lsp-bridge
;;                  :build (:not compile)))
;;   (package! markdown-mode)
;;   (package! yasnippet))

(package! copilot
  :recipe (:host github :repo "copilot-emacs/copilot.el" :files ("*.el")))
