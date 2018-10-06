;;; private/amos/+bindings.el -*- lexical-binding: t; -*-

(require 'general)
(general-override-mode)

(general-define-key
 :keymaps 'override
 :states '(normal insert visual emacs)
 "C-h d"         (lambda!
                  (xref-find-definitions
                   (let* ((backend (xref-find-backend)))
                     (completing-read "Find definitions of: "
                                      (xref-backend-identifier-completion-table backend)
                                      nil nil nil
                                      'xref--read-identifier-history nil))))
 "<f12>"         #'+amos/reset-cursor
 "<f11>"         #'+amos/dump-evil-jump-list
 "M-x"           #'execute-extended-command
 "M-X"           #'+amos/exec-shell-command
 "C-M-u"         #'+amos/avy-open-url
 "<f1>"          #'+amos/reset-zoom
 "<f2>"          #'+amos/decrease-zoom
 "<f3>"          #'+amos/increase-zoom
 "S-<insert>"    #'+amos/paste-from-gui
 "M-1"           #'+amos/workspace-switch-to-1
 "M-2"           #'+amos/workspace-switch-to-2
 "M-3"           #'+amos/workspace-switch-to-3
 "M-4"           #'+amos/workspace-switch-to-4
 "M-5"           #'+amos/workspace-switch-to-5
 "M-6"           #'+amos/workspace-switch-to-6
 "M-7"           #'+amos/workspace-switch-to-7
 "M-8"           #'+amos/workspace-switch-to-8
 "M-9"           #'+amos/workspace-switch-to-9
 "S-<f9>"        #'+amos/workspace-switch-left
 "S-<f10>"       #'+amos/workspace-switch-right
 "C-,"           #'+amos/workspace-switch-left
 "C-."           #'+amos/workspace-switch-right
 "C-<comma>"     #'+amos/workspace-switch-left
 "C-<period>"    #'+amos/workspace-switch-right)

;; Q U M H
(map!
 :gn "M-W"                   #'+amos/kill-current-buffer
 :gn "M-w"                   #'+amos/wipe-current-buffer
 :gn "C-w"                   #'+amos/close-current-buffer ;; bury
 :gniv "M-m"                 #'+amos/switch-buffer
 :n "%"                      #'anzu-multiedit
 :n "R"                      (lambda! (evil-multiedit-match-all) (iedit-show/hide-unmatched-lines))
 :nv "G"                     #'+amos/evil-goto-line
 :n "M-RET"                  #'+amos/toggle-mc
 :i "M-RET"                  #'+amos/close-block
 :n "M-a"                    #'+amos/mark-whole-buffer
 :n "M-g"                    #'+amos/counsel-jumpdir-function
 :i "M-i"                    #'yas-insert-snippet
 :n "M-,"                    #'flycheck-previous-error
 :n "M-."                    #'flycheck-next-error
 :n "M-p"                    #'evil-multiedit-match-symbol-and-prev
 :n "M-n"                    #'evil-multiedit-match-symbol-and-next
 :v "M-p"                    #'evil-multiedit-match-and-prev
 :v "M-n"                    #'evil-multiedit-match-and-next
 :i "M-n"                    #'next-line
 :i "M-p"                    #'previous-line
 :m "N"                      #'evil-ex-search-previous
 :m "W"                      #'+amos/evil-forward-subword-begin
 :m "E"                      #'+amos/evil-forward-subword-end
 :m "B"                      #'+amos/evil-backward-subword-begin
 :ni "M-b"                   #'+amos/backward-word-insert
 :ni "M-B"                   #'+amos/backward-subword-insert
 :ni "M-f"                   #'+amos/forward-word-insert
 :ni "M-F"                   #'+amos/forward-subword-insert
 :ni "M-d"                   #'+amos/forward-delete-word
 :ni "M-D"                   #'+amos/forward-delete-subword
 :ni [M-backspace]           #'+amos/backward-delete-word
 :ni [134217855]             #'+amos/backward-delete-word ; M-DEL
 :ni "S-<f7>"                #'+amos/backward-delete-subword
 :ni "M-S-<backspace>"       #'+amos/backward-delete-subword
 :i "DEL"                    #'+amos/backward-delete-char
 :i "C-w"                    #'+amos/backward-delete-word
 :i "M-r"                    #'sp-slurp-hybrid-sexp
 :i "M-R"                    #'sp-forward-barf-sexp
 :n "M-e"                    #'counsel-dash-at-point
 :n "M-i"                    #'yasdcv-translate-at-point
 :v "M-i"                    #'+amos/evil-visual-insert-snippet
 ;; :n "M-o"                    #'+amos/dired-jump
 ;; :ni "M-o"                   #'downcase-word
 :genvi "M-h"                #'evil-window-left
 :genvi "M-j"                #'evil-window-down
 :genvi "M-k"                #'evil-window-up
 :genvi "M-l"                #'evil-window-right
 :n "C-p"                    #'+amos/counsel-projectile-switch-project
 :nv "C-f"                   #'+amos/avy-goto-char-timer
 :n "C-l"                    #'+amos/redisplay-and-recenter
 :n "C-s"                    #'swiper
 :n "C-S-s"                  #'+amos/counsel-rg-projectile ;; gui
 :n "C-S-d"                  #'+amos/counsel-rg-cur-dir ;; gui
 :nv "C-S-f"                 #'+amos/format-buffer ;; gui
 :n "S-<f4>"                 #'+amos/counsel-rg-projectile ;; terminal
 :n "S-<f5>"                 #'+amos/counsel-rg-cur-dir ;; terminal
 :nv "S-<f11>"               #'+amos/format-buffer ;; terminal
 :n "C-y"                    #'+amos/yank-buffer-filename-with-line-position
 :i "C-y"                    (lambda! (let ((kill-ring my-kill-ring)) (yank)))
 :i "M-y"                    (lambda! (let ((kill-ring my-kill-ring)) (yank-pop)))
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
 :i "M-."                    #'+amos/insert-eol-and-return
 :i "M-e"                    #'smart-forward
 :i "M-a"                    #'smart-backward
 :i "C-u"                    #'+amos/backward-kill-to-bol-and-indent
 :i "C-o"                    #'+amos/kill-line
 :i "C-n"                    #'next-line
 :i "C-p"                    #'previous-line
 :i "C-d"                    #'+amos/delete-char
 :n "C-j"                    #'move-text-down
 :n "C-k"                    #'move-text-up
 :n "C-SPC"                  #'+amos/other-window
 :i "C-SPC"                  #'+amos/complete
 :i "C-s"                    #'+amos/complete-filter
 :i "C-j"                    #'company-dabbrev-code
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
 :v "@"                      #'+evil:macro-on-all-lines
 :n "g@"                     #'+evil:macro-on-all-lines
 :n "gx"                     #'evil-exchange
 :n "gf"                     #'+amos/find-file-at-point
 :n "gd"                     #'+lookup/definition
 :n "go"                     #'+amos/evil-insert-line-below
 :n "gO"                     #'+amos/evil-insert-line-above
 :n "gp"                     #'+evil/reselect-paste
 :n "gr"                     #'+lookup/references
 :v "gR"                     #'+eval:replace-region
 :m "gy"                     #'evil-commentary-yank
 :m "gc"                     #'evil-commentary
 :m "gl"                     #'evil-commentary-line
 :m "gs"                     (lambda! (evil-goto-mark ?s))
 :m "gb"                     (lambda! (evil-goto-mark ?b))
 :m "gm"                     (lambda! (evil-goto-mark ?m))
 :m "g<"                     (lambda! (evil-goto-mark ?<))
 :m "g>"                     (lambda! (evil-goto-mark ?>))
 :m "g."                     #'goto-last-change
 :n ",,"                     #'+amos/projectile-find-other-file

 (:prefix "C-x"
   :g "1"       #'zygospore-toggle-delete-other-windows
   :g "e"       #'pp-eval-last-sexp
   :g "d"       #'+amos/direnv-reload
   :g "a"       #'direnv-edit
   :g "C-r"     #'+amos/replace-last-sexp
   :i "C-f"     #'amos-company-files
   :i "C-n"     #'company-dabbrev-code
   :i "C-p"     #'+company/dabbrev-code-previous
   :g "u"       #'link-hint-open-link
   :g "c"       #'+amos/workspace-new
   :g "C"       #'+amos/workspace-new-scratch
   :g "k"       #'+amos/workspace-delete
   :g "o"       #'+amos/tmux-fork-window
   :gnemv "r"   #'+amos/tmux-source
   :g "C-c"     #'+amos/tmux-detach
   :g "p"       #'doom/other-popup)

 (:prefix "C-c"
   "C-SPC" #'easy-hugo)

 (:prefix "SPC"
   :desc "Toggle mc"                       :nv "m"   #'+amos/toggle-mc
   ;; :desc "Toggle mc"                       :nv "SPC" #'+amos/dired-jump
   :desc "Toggle mc"                       :nv "SPC" #'counsel-find-file
   :desc "Find file in project"            :nv "."   #'+amos/projectile-find-file
   :desc "Find file in project (no cache)" :nv ">"   #'+amos/projectile-find-file-no-cache
   :desc "Find recent file"                :nv ","   #'counsel-recentf
   :desc "Find recent file (no cache)"     :nv "<"   #'+amos/counsel-recentf-no-cache
   :desc "Shell command"                   :n  "e"   #'shell-command
   :desc "Shell command replace"           :v  "e"   #'+amos/shell-command-replace
   :desc "Blink cursor line"               :nv "DEL" #'doom/open-scratch-buffer
   :desc "Elisp command"                   :nv "RET" #'eval-expression
   :desc "Ivy resume"                      :nv "r"   #'ivy-resume
   :desc "Universal argument"              :nv "u"   #'universal-argument
   :desc "Save current file"               :nv "w"   #'save-buffer
   :desc "Next diff hunk"                  :nv "j"   #'git-gutter:next-hunk
   :desc "Previous diff hunk"              :nv "k"   #'git-gutter:previous-hunk
   :desc "Switch workspace buffer"         :nv "b"   #'switch-to-buffer
   :desc "Comment"                         :nv "l"   #'evil-commentary-line

   (:desc "file" :prefix "f"
     :desc "File file"                     :nv "f" #'find-file
     :desc "Open project editorconfig"     :nv "c" #'editorconfig-find-current-editorconfig
     :desc "Delete current file"           :nv "D" #'+evil:delete-this-file
     :desc "Browse emacs.d"                :nv "E" #'doom/sudo-this-file
     :desc "Recent project files"          :nv "R" #'+amos/rename-current-buffer-file
     :desc "Yank filename"                 :nv "y" #'+amos/yank-buffer-filename
     :desc "Yank filename"                 :nv "Y" #'+amos/yank-buffer-filename-nondir)

   (:desc "git" :prefix "g"
     :desc "Git status"                    :nv "s" #'magit-status
     :desc "Git blame"                     :nv "b" #'magit-blame
     :desc "Git timemachine"               :nv "t" #'git-timemachine
     :desc "Git popup hunk"                :nv "p" #'git-gutter:popup-hunk
     :desc "Git revert hunk"               :nv "r" #'git-gutter:revert-hunk
     :desc "Git revert buffer"             :nv "R" #'vc-revert)

   (:desc "help" :prefix "h"
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

   (:desc "notes" :prefix "n"
     :desc "Rust playground"               :nv "r" #'rust-playground
     :desc "Go playground"                 :nv "g" #'go-playground
     :desc "C++ playground"                :nv "c" (lambda! (+amos/workspace-new) (cc-playground))
     :desc "C++ playground"                :nv "l" #'cc-playground-find-snippet
     :desc "Elisp playground"              :nv "e" #'+amos/new-empty-elisp-buffer
     :desc "Open org agenda"               :nv "a" #'+amos/list-todo
     :desc "Open projectile todo"          :nv "t" #'+amos/projectile-todo
     :desc "Browse script"                 :nv "s" #'+amos/browse-script
     :desc "Browse org"                    :nv "o" #'+amos/browse-org
     :desc "Browse note"                   :nv "n" #'+amos/browse-note
     :desc "Browse mode notes"             :nv "m" #'+org/browse-notes-for-major-mode
     :desc "Browse project notes"          :nv "p" #'+org/browse-notes-for-project)

   :desc "Dired"                           :nv "o" #'+amos/dired-jump

   (:desc "quit" :prefix "q"
     :desc "Quit"                          :nv "q" #'+amos/prompt-kill-emacs)

   (:desc "snippets" :prefix "s"
     :desc "New snippet"                   :nv "n" #'yas-new-snippet
     :desc "Insert snippet"                :nv "i" #'yas-insert-snippet
     :desc "Find snippet for mode"         :nv "s" #'yas-visit-snippet-file)

   (:desc "toggle" :prefix "t"
     :desc "Flycheck"                      :nv "f" #'flycheck-mode
     :desc "Rainbow"                       :nv "r" #'rainbow-mode
     :desc "Truncate lines"                :nv "l" #'toggle-truncate-lines
     :desc "Line numbers"                  :nv "n" #'doom/toggle-line-numbers
     :desc "Whitespace"                    :nv "w" #'whitespace-mode
     :desc "Fullscreen"                    :nv "f" #'doom/toggle-fullscreen
     :desc "Indent guides"                 :nv "i" #'highlight-indentation-mode
     :desc "Indent guides (column)"        :nv "I" #'highlight-indentation-current-column-mode))

 :m "gs" #'+evil/easymotion  ; lazy-load `evil-easymotion'
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

 (:after company
   :map company-active-map
   "C-v"        #'company-next-page
   "M-v"        #'company-previous-page
   "C-j"        #'company-select-next
   "C-k"        #'company-select-previous
   "C-h"        #'company-quickhelp-manual-begin
   "C-S-h"      #'company-show-doc-buffer
   "C-S-s"      #'company-search-candidates
   "C-s"        #'company-filter-candidates
   "C-SPC"      #'company-complete-common
   "C-h"        #'company-quickhelp-manual-begin
   "<"          nil
   ">"          nil
   "("          nil
   ")"          nil
   "{"          nil
   "}"          nil
   "C-w"        nil
   "RET"        nil
   "SPC"        nil
   [return]     nil
   [tab]        nil
   [escape]     #'+amos/company-abort

   :map company-search-map
   "C-i"        #'company-complete-selection
   "C-j"        #'company-select-next
   "C-k"        #'company-select-previous
   "C-n"        #'company-search-repeat-forward
   "C-p"        #'company-search-repeat-backward
   "C-s"        (lambda! (company-complete-common) (company-filter-candidates))
   "SPC"        #'+amos/company-search-abort
   ";"          #'+amos/company-search-abort
   ":"          #'+amos/company-search-abort
   "<"          #'+amos/company-search-abort
   ">"          #'+amos/company-search-abort
   "{"          #'+amos/company-search-abort
   "}"          #'+amos/company-search-abort
   "("          #'+amos/company-search-abort
   ")"          #'+amos/company-search-abort
   "C-e"        #'+amos/company-search-abort
   [escape]     #'+amos/company-search-abort)

 (:after swiper
   :map swiper-map
   "C-c o"    #'+ivy/wgrep-occur
   "C-c C-o"  #'+ivy/wgrep-occur)

 (:after counsel
   :map counsel-ag-map
   "C-c o"    #'+ivy/wgrep-occur
   "C-c C-o"  #'+ivy/wgrep-occur
   "C-i"      #'ivy-call
   "C-SPC"    #'counsel-git-grep-recenter
   "M-RET"    (+ivy-do-action! #'+ivy-git-grep-other-window-action))

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
   :n "SPC o" #'quit-window
   :n "M-o" #'quit-window
   :n "C-f" #'dired-omit-mode
   :n "C-i" #'peep-dired-toggle
   :n "C-v" #'peep-dired-scroll-page-down
   :n "E"   #'wdired-change-to-wdired-mode
   :n "I"   #'dired-kill-subdir
   :n "M-n" #'+amos/counsel-jumpfile-function
   :n "M-v" #'peep-dired-scroll-page-up
   :n "S"   #'hydra-dired-quick-sort/body
   :n "S"   #'hydra-dired-quick-sort/body
   :n "W"   (lambda! (dired-copy-filename-as-kill 0))
   :n "Y"   #'+amos/dired-rsync
   :n "c"   (lambda! (dired-ranger-copy t))
   :n "d"   #'dired-flag-file-deletion
   :n "f"   #'counsel-find-file
   :n "h"   #'+amos/up-directory
   :n "j"   #'dired-next-line
   :n "k"   #'dired-previous-line
   :n "p"   #'dired-ranger-paste
   :n "r"   #'dired-ranger-move
   :n "y"   (lambda! (dired-ranger-copy nil))
   :n "RET" #'dired-open-file
   :n "l"   #'dired-open-file)

 (:map emacs-lisp-mode-map
   "C-x e"      #'macrostep-expand
   "#"          #'endless/sharp
   :n "M-r"     #'+eval/buffer
   :n "M-R"     #'+eval/region-and-replace
   ;; :i "M-RET"   #'lisp-state-toggle-lisp-state
   :ni "M-U"    #'+amos/replace-defun
   :ni "M-u"    #'eval-defun)

 (:after evil-magit
   :map magit-mode-map
   :n "<escape>" nil
   :map (magit-status-mode-map magit-revision-mode-map)
   "SPC" nil
   :n "z" #'magit-stash-popup
   :n "C-j" nil
   :n [tab] #'magit-section-toggle
   :n "C-k" nil)

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
   :map evil-multiedit-state-map
   [backspace] (lambda! (evil-multiedit-toggle-or-restrict-region) (evil-multiedit-prev))
   "DEL" (lambda! (evil-multiedit-toggle-or-restrict-region) (evil-multiedit-prev))
   "RET" (lambda! (evil-multiedit-toggle-or-restrict-region) (evil-multiedit-next))
   "j"   #'evil-multiedit-next
   "k"   #'evil-multiedit-prev
   "C-f" #'iedit-show/hide-unmatched-lines
   "#"   #'iedit-number-occurrences
   "$"   (lambda! (evil-multiedit--goto-overlay-end) (backward-char))
   :map (evil-multiedit-state-map iedit-mode-occurrence-keymap)
   "M-p" #'evil-multiedit-match-and-prev
   "M-n" #'evil-multiedit-match-and-next
   "C-n" #'evil-multiedit-next
   "C-p" #'evil-multiedit-prev
   :map evil-multiedit-insert-state-map
   "C-e" #'evil-multiedit--end-of-line
   "C-a" #'evil-multiedit--beginning-of-line)

 ;; ivy
 (:after ivy
   :map ivy-minibuffer-map
   "C-a"    #'move-beginning-of-line
   "C-b"    #'backward-char
   "C-d"    #'+amos/delete-char
   "C-j"    #'ivy-next-line
   "C-k"    #'ivy-previous-line
   "C-l"    #'ivy-alt-done
   "C-o"    #'+amos/kill-line
   "C-r"    #'evil-paste-from-register
   "C-u"    #'+amos/backward-kill-to-bol-and-indent
   "C-w"    #'ivy-yank-word
   "C-y"    (lambda! (let ((kill-ring my-kill-ring)) (yank)))
   "M-y"    (lambda! (let ((kill-ring my-kill-ring)) (yank-pop)))
   "M-b"    #'+amos/backward-word-insert
   "M-d"    #'+amos/forward-delete-word
   "M-f"    #'+amos/forward-word-insert
   "M-B"    #'+amos/backward-subword-insert
   "M-D"    #'+amos/forward-delete-subword
   "M-F"    #'+amos/forward-subword-insert
   "M-g"    #'+amos/ivy-complete-dir
   "M-j"    #'ivy-immediate-done
   "M-r"    #'ivy-toggle-fuzzy
   "M-z"    #'undo
   "TAB"    #'ivy-call
   [escape] #'keyboard-escape-quit
   ;; ivy has its own logic for these
   ;; "DEL"         #'+amos/backward-delete-char
   ;; [M-backspace] #'+amos/backward-delete-word
   ;; [134217855]   #'+amos/backward-delete-word ; M-DEL

   :map ivy-occur-grep-mode-map
   "C-d"    nil
   "d"      #'ivy-occur-delete-candidate)

 ;; yasnippet
 (:after yasnippet
   :map yas-keymap
   "C-l"           #'yas-next-field
   "C-e"           #'+amos/clear-yasnippet

   :map yas-minor-mode-map
   :i "C-l" yas-maybe-expand)

 (:after ccls
   (:map ccls-tree-mode-map
     :m "C-i"      #'ccls-tree-toggle-expand
     :n "c"        #'ccls-tree-toggle-calling
     :n "f"        #'ccls-tree-press
     :n "h"        #'ccls-tree-collapse-or-select-parent
     :n "j"        #'ccls-tree-next-line
     :n "k"        #'ccls-tree-prev-line
     :n "J"        #'ccls-tree-next-sibling
     :n "K"        #'ccls-tree-prev-sibling
     :n "l"        #'ccls-tree-expand-or-set-root
     :n "oh"       #'ccls-tree-press-and-horizontal-split
     :n "ov"       #'ccls-tree-press-and-vertical-split
     :n "oo"       #'ccls-tree-press-and-switch
     :n "q"        #'ccls-tree-quit
     :n "<escape>" #'ccls-tree-quit
     :n "Q"        #'quit-window
     :n "yy"       #'ccls-tree-yank-path
     :n "RET"      #'ccls-tree-press-and-switch
     :n "<left>"   #'ccls-tree-collapse-or-select-parent
     :n "<right>"  #'ccls-tree-expand-or-set-root))

 (:after debug
   ;; For elisp debugging
   :map debugger-mode-map
   :n "q"   (lambda! (top-level) (doom/kill-this-buffer))
   :n "RET" #'debug-help-follow
   :n "e"   #'debugger-eval-expression
   :n "n"   #'debugger-step-through
   :n "c"   #'debugger-continue)

 ;; (:after compile
 ;;   :map compilation-mode-map
 ;;   "SPC" nil
 ;;   "0" nil
 ;;   "1" nil
 ;;   "2" nil
 ;;   "3" nil
 ;;   "4" nil
 ;;   "5" nil
 ;;   "6" nil
 ;;   "7" nil
 ;;   "8" nil
 ;;   "9" nil
 ;;   "g" nil)

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
   :gn "d"        #'edebug-backtrace
   :gn "-"        #'negative-argument
   :gn "="        #'edebug-temp-display-freq-count)

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
   :gn "q" #'+amos/kill-current-buffer)

 (:after magit-blame
   :map magit-blame-read-only-mode-map
   "SPC" nil
   :gn [return]  #'magit-show-commit
   :gn "k" #'magit-blame-previous-chunk
   :gn "K" #'magit-blame-previous-chunk-same-commit
   :gn "j" #'magit-blame-next-chunk
   :gn "J" #'magit-blame-next-chunk-same-commit
   :gn "i" #'magit-blame-toggle-headings
   :gn "q" #'magit-blame-quit)

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

 (:map key-translation-map
   "\035"          (kbd "<escape>")
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
   "C-n"         #'next-line-or-history-element
   "C-p"         #'previous-line-or-history-element
   "C-a"         #'move-beginning-of-line
   "C-b"         #'backward-char
   "C-d"         #'+amos/delete-char
   "C-o"         #'+amos/kill-line
   "C-r"         #'evil-paste-from-register
   "C-u"         #'+amos/backward-kill-to-bol-and-indent
   "C-y"         (lambda! (let ((kill-ring my-kill-ring)) (yank)))
   "M-y"         (lambda! (let ((kill-ring my-kill-ring)) (yank-pop)))
   "M-b"         #'+amos/backward-word-insert
   "M-d"         #'+amos/forward-delete-word
   "M-f"         #'+amos/forward-word-insert
   "M-B"         #'+amos/backward-subword-insert
   "M-D"         #'+amos/forward-delete-subword
   "M-F"         #'+amos/forward-subword-insert
   "M-z"         #'undo
   "DEL"         #'+amos/backward-delete-char
   [M-backspace] #'+amos/backward-delete-word
   [134217855]   #'+amos/backward-delete-word ; M-DEL
   )

 ;; --- Custom evil text-objects ---------------------
 :textobj "a" #'evil-inner-arg                    #'evil-outer-arg
 :textobj "j" #'+amos/any-object-inner            #'+amos/any-object-outer
 :textobj "i" #'evil-indent-plus-i-indent         #'evil-indent-plus-a-indent
 :textobj "I" #'evil-indent-plus-i-indent-up      #'evil-indent-plus-a-indent-up
 :textobj "J" #'evil-indent-plus-i-indent-up-down #'evil-indent-plus-a-indent-up-down)

(after! company
  (bind-keys :map company-active-map
             :filter (company-explicit-action-p)
             ("<" . nil)
             (">" . nil)
             ("C-i" . company-complete-selection)
             ("C-h" . company-quickhelp-manual-begin)))

(map! :map* (sh-mode-map
             fish-mode-map
             python-mode-map
             lua-mode-map
             perl-mode-map)
      :ni [C-return] (lambda! (evil-normal-state) (compile (buffer-file-name) t)))
