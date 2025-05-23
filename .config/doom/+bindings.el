;;; private/amos/+bindings.el -*- lexical-binding: t; no-byte-compile: t; -*-

(setq evil-collection-key-blacklist
      (list "gd" "gf" "K" "[" "]" "gz" "<escape>"
            doom-leader-key doom-localleader-key
            doom-leader-alt-key doom-localleader-alt-key))

(defadvice! +default-evil-collection-disable-blacklist-a (orig-fn)
  :around #'evil-collection-vterm-toggle-send-escape  ; allow binding to ESC
  (let (evil-collection-key-blacklist)
    (apply orig-fn)))

(map! :leader
      :desc "Sticky"                          :nv "DEL" #'evil-sticky-state
      :desc "Resume"                          :nv "SPC" #'vertico-suspend
      :desc "Find file in project"            :nv "."   #'+amos/consult-find
      :desc "Find file in current directory"  :nv ">"   #'+amos/consult-find-cur-dir
      :desc "Find recent file"                :nv ","   #'+amos/consult-recentf
      :desc "Find recent file (no cache)"     :nv "<"   #'+amos/consult-recentf-no-cache
      :desc "Shell command replace"           :nv "e"   #'+amos/shell-command-or-region
      :desc "Shell command"                   :nv "E"   #'+amos/shell-command-on-buffer
      :desc "Elisp command"                   :nv "RET" #'eval-expression
      :desc "Revert buffer"                   :nv "r"   #'+amos/revert-buffer
      :desc "Revert buffers"                  :nv "p"   #'+amos/revert-projectile-buffers
      :desc "Revert buffers"                  :nv "R"   #'+amos/revert-all-buffers
      :desc "Universal argument"              :nv "u"   #'universal-argument
      :desc "Save current file"               :nv "w"   #'save-buffer
      :desc "Next diff hunk"                  :nv "j"   #'git-gutter:next-hunk
      :desc "Previous diff hunk"              :nv "k"   #'git-gutter:previous-hunk
      :desc "Switch workspace buffer"         :nv "b"   #'switch-to-buffer
      :desc "Comment"                         :nv "l"   #'evilnc-comment-or-uncomment-lines
      :desc "Dired"                           :nv "o"   #'+amos/dired-jump
      :desc "Scratch"                         :nv "i"   (cmd! (switch-to-buffer "*scratch*"))

      (:prefix "f"
        :desc "File file"                     :nv "f" #'find-file
        :desc "Open project editorconfig"     :nv "c" #'editorconfig-find-current-editorconfig
        :desc "Delete current file"           :nv "d" #'+evil:delete-this-file
        :desc "Browse emacs.d"                :nv "e" #'doom/sudo-this-file
        :desc "Recent project files"          :nv "r" #'+amos/rename-current-buffer-file
        :desc "Delete current file"           :nv "D" #'+evil:delete-this-file
        :desc "Browse emacs.d"                :nv "E" #'doom/sudo-this-file
        :desc "Recent project files"          :nv "R" #'+amos/rename-current-buffer-file
        :desc "Yank filename"                 :nv "y" #'+amos/yank-buffer-filename
        :desc "Yank filename"                 :nv "Y" #'+amos/yank-buffer-filename-nondir)

      (:prefix "g"
        :desc "Git status"                    :nv "s" #'magit-status
        :desc "Git blame"                     :nv "b" #'magit-blame
        :desc "Git timemachine"               :nv "d" #'+amos/git-timemachine-ediff-current-revision
        :desc "Git timemachine"               :nv "t" #'git-timemachine
        :desc "Git popup hunk"                :nv "p" #'git-gutter:popup-hunk
        :desc "Git revert hunk"               :nv "r" #'git-gutter:revert-hunk
        :desc "Git revert buffer"             :nv "R" #'vc-revert)

      (:prefix "h"
        :desc "Apropos"                       :nv "a" #'apropos
        :desc "Reload theme"                  :nv "R" #'doom/reload-theme
        :desc "Find library"                  :nv "l" #'find-library
        :desc "Toggle Emacs log"              :nv "m" #'doom/popup-toggle-messages
        :desc "Command log"                   :nv "L" #'global-command-log-mode
        :desc "Describe function"             :nv "f" #'describe-function
        :desc "Describe key"                  :nv "k" #'describe-key
        :desc "Describe char"                 :nv "c" #'describe-char
        :desc "Describe mode"                 :nv "M" #'describe-mode
        :desc "Describe variable"             :nv "v" #'describe-variable
        :desc "Describe face"                 :nv "F" #'describe-face
        :desc "Describe DOOM setting"         :nv "s" #'doom/describe-setting
        :desc "Describe DOOM module"          :nv "d" #'doom/describe-module
        :desc "What face"                     :nv "'" #'doom/what-face
        :desc "What minor modes"              :nv ";" #'doom/what-minor-mode
        :desc "Info"                          :nv "i" #'info
        :desc "Toggle profiler"               :nv "p" #'doom/toggle-profiler)

      (:prefix "n"
        :desc "Rust playground"               :nv "r" #'rust-playground
        :desc "Go playground"                 :nv "g" #'go-playground
        :desc "C++ playground"                :nv "c" (cmd! (+amos/workspace-new) (cc-playground))
        :desc "C++ playground"                :nv "l" #'cc-playground-find-snippet
        :desc "Elisp playground"              :nv "e" #'+amos/new-empty-elisp-buffer
        :desc "Open org agenda"               :nv "a" #'+amos/list-todo
        :desc "Open projectile todo"          :nv "t" #'+amos/projectile-todo
        :desc "Browse script"                 :nv "s" #'+amos/browse-script
        :desc "Browse org"                    :nv "o" #'+amos/browse-org
        :desc "Browse note"                   :nv "n" #'+amos/browse-note
        :desc "Browse mode notes"             :nv "m" #'+org/browse-notes-for-major-mode
        :desc "Browse project notes"          :nv "p" #'+org/browse-notes-for-project)

      (:prefix "q"
        :desc "Quit"                          :nv "q" #'+amos/prompt-kill-emacs)

      (:prefix "s"
        :desc "New snippet"                   :nv "n" #'yas-new-snippet
        :desc "Insert snippet"                :nv "i" #'yas-insert-snippet
        :desc "Find snippet for mode"         :nv "s" #'yas-visit-snippet-file)

      (:prefix "t"
        :desc "Flycheck"                      :nv "f" #'flycheck-mode
        :desc "Rainbow"                       :nv "r" #'rainbow-mode
        :desc "Truncate lines"                :nv "l" #'toggle-truncate-lines
        :desc "Line numbers"                  :nv "n" #'doom/toggle-line-numbers
        :desc "Whitespace"                    :nv "w" #'whitespace-mode
        :desc "Fullscreen"                    :nv "f" #'doom/toggle-fullscreen
        :desc "Indent guides"                 :nv "i" #'highlight-indentation-mode
        :desc "Indent guides (column)"        :nv "I" #'highlight-indentation-current-column-mode))

;; Q U M H
(map! (:map override
        ;; :n "<escape>"          #'+amos/evil-force-normal-state
        :gnemv "C-h d"         (cmd!
                                (xref-find-definitions
                                 (let* ((backend (xref-find-backend)))
                                   (completing-read "Find definitions of: "
                                                    (xref-backend-identifier-completion-table backend)
                                                    nil nil nil
                                                    'xref--read-identifier-history nil))))
        :gnemv "<xterm-paste>" #'+amos/xterm-paste
        ;; :gnemv "<f12>"         #'+amos/reset-cursor
        ;; :gnemv "<f11>"         #'+amos/dump-evil-jump-list
        :gnemv "C-M-u"         #'+amos/avy-open-url
        :gnemv "M-x"           #'execute-extended-command
        :gnemv "<f1>"          #'+amos/reset-zoom
        :gnemv "<f2>"          #'+amos/decrease-zoom
        :gnemv "<f3>"          #'+amos/increase-zoom
        :gnemv "S-<insert>"    #'+amos/paste-from-gui
        :gnemv "M-1"           #'+amos/workspace-switch-to-1
        :gnemv "M-2"           #'+amos/workspace-switch-to-2
        :gnemv "M-3"           #'+amos/workspace-switch-to-3
        :gnemv "M-4"           #'+amos/workspace-switch-to-4
        :gnemv "M-5"           #'+amos/workspace-switch-to-5
        :gnemv "M-6"           #'+amos/workspace-switch-to-6
        :gnemv "M-7"           #'+amos/workspace-switch-to-7
        :gnemv "M-8"           #'+amos/workspace-switch-to-8
        :gnemv "M-9"           #'+amos/workspace-switch-to-9
        ;; :gnemv "M-="           #'+amos/workspace-switch-to-htop
        :gnemv "S-<f9>"        #'+amos/workspace-switch-left
        :gnemv "S-<f10>"       #'+amos/workspace-switch-right
        :gnemv "C-9"           #'+amos/workspace-switch-left
        :gnemv "C-0"           #'+amos/workspace-switch-right
        :gnemv "S-<f1>"        #'centaur-tabs-backward-tab
        :gnemv "S-<f2>"        #'centaur-tabs-forward-tab
        :gnemv "C-,"           #'centaur-tabs-backward-tab
        :gnemv "C-."           #'centaur-tabs-forward-tab
        :gnemv "C-<comma>"     #'centaur-tabs-backward-tab
        :gnemv "C-<period>"    #'centaur-tabs-forward-tab
        )

      :gn "M-W"                   #'+amos/kill-current-buffer
      :gn "M-w"                   #'+amos/wipe-current-buffer
      :gn "C-w"                   #'+amos/close-current-buffer ;; bury
      :gniv "M-m"                 #'+amos/switch-buffer
      :n "%"                      #'anzu-multiedit
      :n "R"                      (cmd! (evil-multiedit-match-all) (iedit-show/hide-unmatched-lines))
      :nv "G"                     #'+amos/evil-goto-line
      :n "M-RET"                  #'+amos/toggle-mc
      :i "M-RET"                  #'+amos/close-block
      :n "M-a"                    #'+amos/mark-whole-buffer
      :n "M-g"                    #'+amos/consult-jumpdir-function
      :i "M-i"                    #'yas-insert-snippet
      ;; :n "M-,"                    #'+amos/flycheck-previous-error
      ;; :n "M-."                    #'+amos/flycheck-next-error
      :n "M-,"                    #'+amos/flymake-goto-prev-error
      :n "M-."                    #'+amos/flymake-goto-next-error
      :n "M-p"                    #'evil-multiedit-match-symbol-and-prev
      :n "M-n"                    #'evil-multiedit-match-symbol-and-next
      :n "M-y"                    #'+amos/yank-flymake-error
      :v "M-p"                    #'evil-multiedit-match-and-prev
      :v "M-n"                    #'evil-multiedit-match-and-next
      :i "M-n"                    #'next-line
      :i "M-p"                    #'previous-line
      :m "c"                      #'+amos/evil-change
      :m "-"                      #'evil-end-of-line
      :m "N"                      #'evil-ex-search-previous
      :m "W"                      #'+amos/evil-forward-subword-begin
      :m "E"                      #'+amos/evil-forward-subword-end
      :m "B"                      #'+amos/evil-backward-subword-begin
      :m "gj"                     #'+amos/evil-next-visual-line
      :m "gk"                     #'+amos/evil-previous-visual-line
      :ni "M-b"                   #'+amos/backward-word-insert
      :ni "M-B"                   #'+amos/backward-subword-insert
      :ni "M-f"                   #'+amos/forward-word-insert
      :ni "M-F"                   #'+amos/forward-subword-insert
      :ni "M-d"                   #'+amos/delete-forward-word
      :ni "M-D"                   #'+amos/delete-forward-subword
      :ni [M-backspace]           #'+amos/delete-backward-word
      :ni [134217855]             #'+amos/delete-backward-word ; M-DEL
      :ni "S-<f7>"                #'+amos/delete-backward-subword
      :ni "M-S-<backspace>"       #'+amos/delete-backward-subword
      :i "DEL"                    #'+amos/delete-backward-char
      :i "C-w"                    #'+amos/delete-backward-word
      :i "M-r"                    #'sp-slurp-hybrid-sexp
      :i "M-R"                    #'sp-forward-barf-sexp
      :i "M-{"                    (cmd! (+amos-surround-with-pair ?} t))
      :i "M-}"                    (cmd! (+amos-surround-with-pair ?}))
      :i "M-("                    (cmd! (+amos-surround-with-pair ?\) t))
      :i "M-)"                    (cmd! (+amos-surround-with-pair ?\)))
      :i "M-,"                    (cmd! (+amos-surround-with-pair ?> t))
      :i "M-."                    (cmd! (+amos-surround-with-pair ?>))
      ;; these will break terminal
      ;; :i "M-["                    (cmd! (+amos-surround-with-pair ?\] t))
      ;; :i "M-]"                    (cmd! (+amos-surround-with-pair ?\]))
      :i "M-'"                    (cmd! (+amos-surround-with-pair ?\'))
      :i "M-_"                    (cmd! (+amos-surround-with-pair ?\' t))
      :i "M-\""                   (cmd! (+amos-surround-with-pair ?\"))
      :i "M-p"                    (cmd! (+amos-surround-with-pair ?\" t))
      :i "M-R"                    #'sp-forward-barf-sexp
      :n "M-e"                    #'+amos/lookup-docsets
      :n "M-i"                    #'yasdcv-translate-at-point
      :v "M-i"                    #'+amos/evil-visual-insert-snippet
      :genvi "M-h"                #'evil-window-left
      :genvi "M-j"                #'+amos/goto-next-error
      :genvi "M-k"                #'+amos/goto-previous-error
      :genvi "M-l"                #'evil-window-right
      :nv "C-f"                   #'+amos/avy-goto-char-timer
      :n "C-l"                    #'+amos/redisplay-and-recenter
      :n "C-s"                    #'+amos/consult-line
      :n "C-S-s"                  #'+amos/consult-ripgrep ;; gui
      :n "C-S-d"                  #'+amos/consult-ripgrep-cur-dir ;; gui
      :nv "C-S-f"                 #'+format/region-or-buffer ;; gui
      :n "S-<f4>"                 #'+amos/consult-ripgrep ;; terminal
      :n "S-<f5>"                 #'+amos/consult-ripgrep-cur-dir ;; terminal
      :nv "S-<f11>"               #'+format/region-or-buffer ;; terminal
      :n "C-y"                    #'+amos/yank-buffer-filename-with-line-position
      :i "M-y"                    #'+amos/yank-pop
      :i "C-a"                    #'evil-beginning-of-line
      :n "C-a"                    #'evil-numbers/inc-at-pt
      :n "C-q"                    #'evil-numbers/dec-at-pt
      :n "M-s"                    #'+amos/line-substitute
      :n "M-S"                    #'+amos/all-substitute
      :v "M-s"                    #'+amos/region-substitute
      :v "C-a"                    #'+amos/ca
      :v "g C-a"                  #'+amos/gca
      :v "C-q"                    #'+amos/cd
      :v "g C-q"                  #'+amos/gcd
      :i [remap newline]          #'newline-and-indent
      :n "C-e"                    #'+amos/maybe-add-end-of-statement
      :i "C-e"                    #'+amos/smart-eol-insert
      :i "M-e"                    #'smart-forward
      :i "M-a"                    #'smart-backward
      :i "C-u"                    #'+amos/backward-kill-to-bol-and-indent
      :i "C-o"                    #'+amos/kill-line
      :i "C-n"                    #'next-line
      :i "C-p"                    #'previous-line
      :i "C-d"                    #'+amos/delete-char
      :n "C-j"                    #'move-text-down
      :n "C-k"                    #'move-text-up
      ;; :n "C-SPC"                  #'+amos/other-window
      ;; :i "C-SPC"                  #'+amos/complete
      ;; :i "C-s"                    #'+amos/complete-filter
      ;; :i "C-j"                    #'company-dabbrev-code
      :v "R"                      #'evil-multiedit-match-all
      :n "!"                      #'rotate-text
      :v "H"                      #'+amos/align-repeat-left
      :v "L"                      #'+amos/align-repeat-right
      :v "u"                      #'undo-tree-undo
      :v "C-r"                    #'undo-tree-redo
      :n "s"                      #'evil-substitute
      :n "S"                      #'evil-change-whole-line
      :v "s"                      #'evil-surround-region
      :v "S"                      #'evil-substitute
      :o "s"                      #'evil-surround-edit
      :v "v"                      #'er/expand-region
      :v "V"                      #'er/contract-region
      :n "p"                      #'evil-paste-after
      :n "P"                      #'evil-paste-before
      :n "K"                      #'evil-paste-pop
      :n "L"                      #'evil-paste-pop-next
      :n "("                      #'+amos/smart-jumper-backward
      :n ")"                      #'+amos/smart-jumper-forward
      :v "<"                      #'+evil/visual-dedent
      :v ">"                      #'+evil/visual-indent
      :n "gx"                     #'evil-exchange
      :n "gf"                     #'find-file-at-point
      :n "gd"                     #'+lookup/definition
      :n "go"                     #'+amos/evil-insert-line-below
      :n "gO"                     #'+amos/evil-insert-line-above
      :m "gb"                     #'+lookup/implementations
      :n "gp"                     #'+evil/reselect-paste
      :n "gr"                     #'+lookup/references
      :v "gR"                     #'+eval:replace-region
      :vnm "gy"                   #'evilnc-copy-and-comment-lines
      :vnm "gl"                   #'evilnc-comment-or-uncomment-lines
      :n "m"                      #'+amos/push-mark
      :n "M"                      (cmd! (+amos/push-mark t))
      ;; :n "M-9"                    #'+amos/counsel-view-marks
      :m "gs"                     (cmd! (evil-goto-mark ?s))
      :m "gm"                     (cmd! (evil-goto-mark ?m))
      :m "g<"                     (cmd! (evil-goto-mark ?<))
      :m "g>"                     (cmd! (evil-goto-mark ?>))
      :m "g."                     #'goto-last-change
      :n ",,"                     #'+amos/find-other-file
      :m "gs"                     #'+evil/easymotion

      (:prefix "C-x"
        :g "1"       #'zygospore-toggle-delete-other-windows
        :g "e"       #'pp-eval-last-sexp
        :g "d"       #'envrc-allow
        :g "a"       #'direnv-edit
        :g "C-r"     #'+amos/replace-last-sexp
        ;; :i "C-f"     #'amos-company-files
        ;; :i "C-n"     #'company-dabbrev-code
        ;; :i "C-p"     #'+company/dabbrev-code-previous
        :g "u"       #'+amos/avy-open-url
        :g "c"       #'+amos/workspace-new
        :g "C"       #'+amos/workspace-new-scratch
        :g "k"       #'+amos/workspace-delete
        :g "C-x"     #'+amos/workspace-delete
        ;; :g "o"       #'+amos/tmux-fork-window
        :g "o"       #'+amos/kitty-fork-window
        :g "l"       #'+amos/launch
        :gnemv "r"   #'+amos/tmux-source
        :g "C-c"     #'+amos/tmux-detach
        :g "p"       #'doom/other-popup)

      (:prefix "C-c"
        ;; "C-SPC" #'easy-hugo)
        "C-SPC" #'+amos/org-journal)

      (:after evil-easymotion
        :map evilem-map
        "a" (evilem-create #'evil-forward-arg)
        "A" (evilem-create #'evil-backward-arg)
        "n" (evilem-create #'evil-ex-search-next)
        "N" (evilem-create #'evil-ex-search-previous)
        "s" (evilem-create #'evil-snipe-repeat
                           :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                           :bind ((evil-snipe-scope 'buffer)
                                  (evil-snipe-enable-highlight)
                                  (evil-snipe-enable-incremental-highlight)))
        "S" (evilem-create #'evil-snipe-repeat-reverse
                           :pre-hook (save-excursion (call-interactively #'evil-snipe-s))
                           :bind ((evil-snipe-scope 'buffer)
                                  (evil-snipe-enable-highlight)
                                  (evil-snipe-enable-incremental-highlight))))

      (:after files
        :map ctl-x-map
        :g "C-d" #'envrc-allow)

      (:after dired
        :map dired-mode-map
        "SPC" nil
        "G"   nil
        "g"   nil
        "e"   nil
        "v"   nil
        "b"   nil
        "n"   nil
        "N"   nil
        "y"   nil
        "C-o" nil
        "C-i" nil
        :n "q"   #'quit-window
        :n "z"   #'dired-do-compress
        :n "C-f" #'dired-omit-mode
        :n "C-i" #'peep-dired-toggle
        :n "C-v" #'peep-dired-scroll-page-down
        :n "E"   #'wdired-change-to-wdired-mode
        :n "I"   #'dired-kill-subdir
        :n "M-n" #'+amos/consult-jumpfile-function
        :n "M-v" #'peep-dired-scroll-page-up
        :n "S"   #'hydra-dired-quick-sort/body
        :nv "d"  #'dired-flag-file-deletion
        :n "D"   #'dired-do-delete
        :n "f"   #'find-file
        :n "F"   #'dired-do-copy
        :n "h"   #'+amos/dired-up-directory
        :n "M-p" #'+amos/dired-up-directory
        :n "M-o" #'+amos/dired-prev-history
        :n "M-i" #'+amos/dired-next-history
        :n "!"   #'dired-do-shell-command
        :n "j"   #'dired-next-line
        :n "k"   #'dired-previous-line
        :n "p"   #'dired-ranger-paste
        :n "P"   #'dired-ranger-move
        :nv "R"  #'dired-do-rename
        :n "x"   #'dired-do-flagged-delete
        :n "w"   #'dired-copy-filename-as-kill
        :n "i"   #'dired-create-directory
        :nv "m"  #'dired-mark
        :nv "u"  #'dired-unmark
        :n "W"   (cmd! (dired-copy-filename-as-kill 0))
        :n "c"   (cmd! (dired-ranger-copy t) (+amos/dired-print-clipboard))
        :n "C"   (cmd! (dired-ranger-copy t) (+amos/dired-copy-to-clipboard) (+amos/dired-print-clipboard))
        :n "y"   (cmd! (dired-ranger-copy nil) (+amos/dired-print-clipboard))
        :n "Y"   (cmd! (dired-ranger-copy nil) (+amos/dired-copy-to-clipboard) (+amos/dired-print-clipboard))
        :n "a"   #'+amos/dired-rsync
        :n "RET" #'dired-open-file
        :n "C-<return>" #'+amos/dired-xdg-open
        :n "l"   #'dired-open-file)

      (:map emacs-lisp-mode-map
        "C-x e"      #'macrostep-expand
        "#"          #'endless/sharp
        :n "M-r"     #'+eval/buffer
        :n "M-R"     #'+eval/region-and-replace
        ;; :i "M-RET"   #'lisp-state-toggle-lisp-state
        :ni "M-U"    #'+amos/replace-defun
        :ni "M-u"    #'eval-defun)

      (:after magit
        :map (magit-status-mode-map magit-revision-mode-map)
        "SPC" nil
        :n "z" #'magit-stash)

      (:after tex
        :map TeX-mode-map
        :i "M-o" #'ivy-bibtex-with-local-bibliography
        :i "M-O" #'+amos/ivy-reftex)

      (:after lsp-ui-imenu
        :map lsp-ui-imenu-mode-map
        :n "q" 'lsp-ui-imenu--kill
        :n "<escape>" 'lsp-ui-imenu--kill
        :n "M-v" 'lsp-ui-imenu--kill
        :n "l" 'lsp-ui-imenu--next-kind
        :n "h" 'lsp-ui-imenu--prev-kind
        :n "C-i" 'lsp-ui-imenu--view
        :n "RET" 'lsp-ui-imenu--visit)

      (:after evil-multiedit
        :map evil-multiedit-mode-map
        :n [backspace] (cmd! (evil-multiedit-toggle-or-restrict-region) (evil-multiedit-prev))
        :n "DEL" (cmd! (evil-multiedit-toggle-or-restrict-region) (evil-multiedit-prev))
        :n "RET" (cmd! (evil-multiedit-toggle-or-restrict-region) (evil-multiedit-next))
        :n "j"   #'evil-multiedit-next
        :n "k"   #'evil-multiedit-prev
        :n "C-f" #'iedit-show/hide-unmatched-lines
        :n "#"   #'iedit-number-occurrences
        :i "C-v" #'+amos/iedit-number-occurrences
        :i "C-e" #'evil-multiedit--end-of-line
        :i "C-a" #'evil-multiedit--beginning-of-line
        :map (evil-multiedit-mode-map iedit-mode-occurrence-keymap)
        :gvn "M-p" #'evil-multiedit-match-and-prev
        :gvn "M-n" #'evil-multiedit-match-and-next
        :gvn "C-n" #'evil-multiedit-next
        :gvn "C-p" #'evil-multiedit-prev)

      (:after vertico
        :map vertico-map
        "M-RET" #'vertico-exit-input
        "C-s"   #'vertico-suspend
        "C-w"   #'+amos/consult-yank-word
        "C-i"   #'+vertico/embark-preview
        "C-j"   #'vertico-next
        "C-k"   #'vertico-previous
        "C-h" (cmds! (eq 'file (vertico--metadata-get 'category)) #'vertico-directory-up)
        "C-l" (cmds! (eq 'file (vertico--metadata-get 'category)) #'+vertico/enter-or-preview)

        :map minibuffer-local-map
        "C-c C-c"           #'embark-act
        "C-c C-o"           #'embark-export
        "C-c C-l"           #'+amos/embark-collect
        :desc "Export to writable buffer" "C-c C-e" #'+amos/embark-export-write
        )

      ;; yasnippet
      (:after yasnippet
        :map yas-keymap
        "C-l"           #'yas-next-field
        "C-e"           #'+amos/clear-yasnippet

        :map yas-minor-mode-map
        :i "C-l" yas-maybe-expand)

      (:after comint
        (:map comint-mode-map
          "C-d" nil))

      (:after debug
        ;; For elisp debugging
        :map debugger-mode-map
        :n "q"   (cmd! (top-level) (doom/kill-this-buffer))
        :n "RET" #'debug-help-follow
        :n "e"   #'debugger-eval-expression
        :n "n"   #'debugger-step-through
        :n "c"   #'debugger-continue)

      (:after edebug
        :map edebug-mode-map
        :gn "s"        #'edebug-step-mode
        :gn "n"        #'edebug-next-mode
        :gn "g"        #'edebug-go-mode
        :gn "G"        #'edebug-Go-nonstop-mode
        :gn "t"        #'edebug-trace-mode
        :gn "T"        #'edebug-Trace-fast-mode
        :gn "c"        #'edebug-continue-mode
        :gn "C"        #'edebug-Continue-fast-mode
        :gn "f"        #'edebug-forward-sexp
        :gn "N"        #'edebug-goto-here
        :gn "I"        #'edebug-instrument-callee
        :gn "i"        #'edebug-step-in
        :gn "o"        #'edebug-step-out
        :gn "q"        #'top-level
        :gn "Q"        #'edebug-top-level-nonstop
        :gn "a"        #'abort-recursive-edit
        :gn "S"        #'edebug-stop
        :gn "b"        #'edebug-set-breakpoint
        :gn "u"        #'edebug-unset-breakpoint
        :gn "B"        #'edebug-next-breakpoint
        :gn "x"        #'edebug-set-conditional-breakpoint
        :gn "X"        #'edebug-set-global-break-condition
        :gn "r"        #'edebug-previous-result
        :gn "e"        #'edebug-eval-expression
        :gn "C-x C-e"  #'edebug-eval-last-sexp
        :gn "E"        #'edebug-visit-eval-list
        :gn "w"        #'edebug-where
        :gn "v"        #'edebug-view-outside ;; maybe obsolete??
        :gn "p"        #'edebug-bounce-point
        :gn "P"        #'edebug-view-outside ;; same as v
        :gn "W"        #'edebug-toggle-save-windows
        :gn "?"        #'edebug-help
        :gn "d"        #'edebug-pop-to-backtrace
        :gn "-"        #'negative-argument
        :gn "="        #'edebug-temp-display-freq-count)

      (:map restclient-mode-map
        :g "C-c C-c"   #'+amos/restclient-http-send-current)

      (:map emacs-lisp-mode-map
        :i "M-o"       #'lisp-state-toggle-lisp-state)

      (:after git-timemachine
        :map git-timemachine-mode-map
        :n "C-k" #'git-timemachine-show-previous-revision
        :n "C-j" #'git-timemachine-show-next-revision
        :n "w"   #'git-timemachine-kill-abbreviated-revision
        :n "W"   #'git-timemachine-kill-revision
        :n "o"   #'git-timemachine-show-revision-fuzzy
        :n "q"   #'git-timemachine-quit
        :n "gb"  #'git-timemachine-blame)

      (:after helpful
        :map helpful-mode-map
        :gn "o" #'link-hint-open-link
        :gn "q" #'+amos/kill-current-buffer)

      (:after magit-blame
        :map magit-blame-read-only-mode-map
        "SPC" nil
        :gn [return]  #'magit-show-commit
        :gn "RET"  #'magit-show-commit
        :gn "k" #'magit-blame-previous-chunk
        :gn "K" #'magit-blame-previous-chunk-same-commit
        :gn "j" #'magit-blame-next-chunk
        :gn "J" #'magit-blame-next-chunk-same-commit
        :gn "i" #'magit-blame-toggle-headings
        :gn "q" #'magit-blame-quit)

      (:after magit-files
        :map magit-blob-mode-map
        "p" nil
        "n" nil
        "b" nil
        "f" nil
        :gn "q" 'magit-kill-this-buffer
        :gn "C-k" 'magit-blob-previous
        :gn "C-j" 'magit-blob-next
        :gn "a" 'magit-blame-addition
        :gn "d" 'magit-blame-removal
        :gn "r" 'magit-blame-reverse)

      (:after magit-status
        :map magit-status-mode-map
        :n " " nil)

      (:after profiler
        :map profiler-report-mode-map
        :n "RET" #'profiler-report-expand-entry)

      (:after evil-snipe
        :map evil-snipe-parent-transient-map
        :g "j" #'evil-snipe-repeat
        :g "k" #'evil-snipe-repeat-reverse)

      (:after re-builder
        :map reb-mode-map
        :g "C-c C-k" #'reb-quit)

      (:map key-translation-map
        "\035"          (kbd "<escape>")
        [C-f5]          (kbd "<tab>")
        [S-iso-lefttab] [backtab]
        "C-1"           (kbd "1")
        "C-2"           (kbd "2")
        "C-3"           (kbd "3")
        "C-4"           (kbd "4")
        "C-5"           (kbd "5")
        "C-6"           (kbd "6")
        "C-7"           (kbd "7")
        "C-8"           (kbd "8")
        "C-9"           (kbd "9")
        "C-0"           (kbd "0")
        "\e[70~"        (kbd "<C-return>")
        "C-@"           (kbd "C-SPC"))

      (:map (minibuffer-local-map
             minibuffer-local-ns-map
             minibuffer-local-completion-map
             minibuffer-local-must-match-map
             minibuffer-local-isearch-map
             minibuffer-local-shell-command-map
             evil-ex-completion-map
             evil-ex-search-keymap
             read-expression-map)
        [escape]      #'abort-recursive-edit
        ;; "C-w"         #'+amos/minibuffer-yank-word
        "C-k"         #'previous-line-or-history-element
        "C-j"         #'next-line-or-history-element
        "C-n"         #'next-line-or-history-element
        "C-p"         #'previous-line-or-history-element
        "C-a"         #'move-beginning-of-line
        "C-b"         #'backward-char
        "C-f"         #'forward-char
        "C-d"         #'+amos/delete-char
        "C-o"         #'+amos/kill-line
        ;; "C-r"         #'counsel-minibuffer-history
        "C-u"         #'+amos/backward-kill-to-bol-and-indent
        "M-y"         #'+amos/yank-pop
        "M-'"         (cmd! (insert ?\') (insert ?\{) (insert ?\}) (insert ?\') (backward-char 2))
        "M-b"         #'+amos/backward-word-insert
        "M-d"         #'+amos/delete-forward-word
        "M-f"         #'+amos/forward-word-insert
        "M-B"         #'+amos/backward-subword-insert
        "M-D"         #'+amos/delete-forward-subword
        "M-F"         #'+amos/forward-subword-insert
        "M-z"         #'undo
        "DEL"         #'+amos/delete-backward-char
        "S-<f7>"      #'+amos/delete-backward-subword
        "S-<insert>"    #'+amos/paste-from-gui
        "M-S-<backspace>"       #'+amos/delete-backward-subword
        [M-backspace] #'+amos/delete-backward-word
        [134217855]   #'+amos/delete-backward-word ; M-DEL
        )

      ;; --- Custom evil text-objects ---------------------
      :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
      :textobj "j" #'+amos/any-object-inner            #'+amos/any-object-outer
      :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
      :textobj "I" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
      :textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down)

(map! :when (modulep! :completion corfu)
      :after corfu
      (:map corfu-map
            [remap corfu-insert-separator] #'+corfu-smart-sep-toggle-escape
            [remap scroll-down-command] nil
            [remap scroll-up-command] nil))
(let ((cmds-del
       `(menu-item "Reset completion" corfu-reset
         :filter ,(lambda (cmd)
                    (when (and (>= corfu--index 0)
                               (eq corfu-preview-current 'insert))
                      cmd))))
      (cmds-ret
       `(menu-item "Insert completion DWIM" corfu-insert
         :filter ,(lambda (cmd)
                    (interactive)
                    (cond ((null +corfu-want-ret-to-confirm)
                           (corfu-quit)
                           nil)
                          ((eq +corfu-want-ret-to-confirm 'minibuffer)
                           (funcall-interactively cmd)
                           nil)
                          ((and (or (not (minibufferp nil t))
                                    (eq +corfu-want-ret-to-confirm t))
                                (>= corfu--index 0))
                           cmd)
                          ((or (not (minibufferp nil t))
                               (eq +corfu-want-ret-to-confirm t))
                           nil)
                          (t cmd))))))
  (map! :when (modulep! :completion corfu)
        :map corfu-mode-map
        :gi "C-SPC" #'completion-at-point
        :map corfu-map
        :gi "C-SPC" #'corfu-insert-separator
        :gi "C-a" #'corfu-prompt-beginning
        :gi "C-o" nil
        :gi "C-u" nil
        :gi "C-d" nil
        [backspace] cmds-del
        "DEL" cmds-del
        :gi [return] cmds-ret
        :gi "RET" cmds-ret))

(defun +amos/run-script () (interactive) (evil-normal-state) (compile (buffer-file-name) t))

(after! sh-script
  (define-key sh-base-mode-map (kbd "<C-return>") #'+amos/run-script))

(after! fish-mode
  (define-key fish-mode-map (kbd "<C-return>") #'+amos/run-script))

(after! python
  (define-key python-base-mode-map (kbd "<C-return>") #'+amos/run-script))

(after! lua-mode
  (define-key lua-mode-map (kbd "<C-return>") #'+amos/run-script))

(after! perl-mode
  (define-key perl-mode-map (kbd "<C-return>") #'+amos/run-script))
