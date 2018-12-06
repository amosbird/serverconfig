function fish_user_key_bindings
    function updir
        cd ..
        commandline -f repaint
        if count $TMUX > /dev/null
            tmux refresh-client -S
        end
        eval (direnv export fish);
    end

    function myprevd
        prevd > /dev/null 2>&1
        commandline -f repaint
        if count $TMUX > /dev/null
            tmux refresh-client -S
        end
        eval (direnv export fish);
    end

    function mynextd
        nextd > /dev/null 2>&1
        commandline -f repaint
        if count $TMUX > /dev/null
            tmux refresh-client -S
        end
        eval (direnv export fish);
    end

    function fzf-jump-cd -d "Change directory"
        set -q FZF_ALT_C_COMMAND; or set -l FZF_ALT_C_COMMAND "command jump top"
        set -q FZF_TMUX_HEIGHT; or set FZF_TMUX_HEIGHT 40%
        begin
            set -lx FZF_DEFAULT_OPTS "--expect f10 --height $FZF_TMUX_HEIGHT --reverse $FZF_DEFAULT_OPTS $FZF_ALT_C_OPTS"
            eval "$FZF_ALT_C_COMMAND | "(__fzfcmd)" +m" | read -aAl result
            set -l dir (string trim $result[2])
            if test -n $dir
                if test -z $result[1]
                    if not cd $dir
                        jump clean
                    end
                else
                    commandline -i $dir"/"
                end
            end
        end
        commandline -f repaint
        if count $TMUX > /dev/null
            tmux refresh-client -S
        end
        eval (direnv export fish);
    end

    function fzf-select -d 'fzf commandline job and print unescaped selection back to commandline'
        set -l cmd (commandline -j)
        set -q FZF_TMUX_HEIGHT; or set FZF_TMUX_HEIGHT 40%
        [ "$cmd" ]; or return
        eval $cmd | eval (__fzfcmd) -m --height $FZF_TMUX_HEIGHT --reverse --tiebreak=index --select-1 --exit-0 | string join ' ' | read -l result
        [ "$result" ]; and commandline -j -- $result
        commandline -f repaint
    end

    function fzf-complete -d 'fzf completion and print selection back to commandline'
        # As of 2.6, fish's "complete" function does not understand
        # subcommands. Instead, we use the same hack as __fish_complete_subcommand and
        # extract the subcommand manually.
        set -l cmd (commandline -co) (commandline -ct)
        switch $cmd[1]
            case env sudo
                for i in (seq 2 (count $cmd))
                    switch $cmd[$i]
                        case '-*'
                        case '*=*'
                        case '*'
                            set cmd $cmd[$i..-1]
                            break
                    end
                end
        end
        set cmd (string join -- ' ' $cmd)

        set -l complist (complete -C$cmd)
        set -l result
        set -q FZF_TMUX_HEIGHT; or set FZF_TMUX_HEIGHT 40%
        string join -- \n $complist | sort | eval (__fzfcmd) -m --height $FZF_TMUX_HEIGHT --reverse --select-1 --exit-0 --header '(commandline)' | cut -f1 | while read -l r; set result $result $r; end
        set prefix (string sub -s 1 -l 1 -- (commandline -t))
        for i in (seq (count $result))
            set -l r $result[$i]
            switch $prefix
                case "'"
                    commandline -t -- (string escape -- $r)
                case '"'
                    if string match '*"*' -- $r >/dev/null
                        commandline -t --  (string escape -- $r)
                    else
                        commandline -t -- '"'$r'"'
                    end
                case '~'
                    commandline -t -- (string sub -s 2 (string escape -n -- $r))
                case '*'
                    commandline -t -- (string escape -n -- $r)
            end
            [ $i -lt (count $result) ]; and commandline -i ' '
        end

        commandline -f repaint
    end

    function fzf-history-widget -d "Show command history"
        set -q FZF_TMUX_HEIGHT; or set FZF_TMUX_HEIGHT 40%
        begin
            set -lx FZF_DEFAULT_OPTS "--read0 --reverse --height $FZF_TMUX_HEIGHT $FZF_DEFAULT_OPTS +s --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS +m"
            history -z |  eval (__fzfcmd) -q '(commandline)' | read -l result
            and commandline -r -- (string trim -r $result)
        end
        commandline -f repaint
    end

    function fzf-history-token-widget -d "Show command history"
        set -q FZF_TMUX_HEIGHT; or set FZF_TMUX_HEIGHT 40%
        set str (commandline -jc)
        set tok (commandline -tc)
        begin
            set -lx FZF_DEFAULT_OPTS "--reverse --height $FZF_TMUX_HEIGHT $FZF_DEFAULT_OPTS +s --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS +m"
            if [ $str = $tok ]
                history -z | eval (__fzfcmd) --read0 -q '$str' | read -lz result
                and commandline -r -- (string trim -r $result)
            else
                string tokenize -n 1000 -a | eval (__fzfcmd) -q '$tok' | read -l result
                and commandline -tr -- $result
            end
        end
        commandline -f repaint
    end

    function elvish-nav -d ""
        tput smcup
        elvish | read -l result
        [ "$result" ]; and cd $result
        tput rmcup
        commandline -f repaint
        eval (direnv export fish);
    end

    function __fzfcmd
        set -q FZF_TMUX; or set FZF_TMUX 0
        set -q FZF_TMUX_HEIGHT; or set FZF_TMUX_HEIGHT 40%
        if [ $FZF_TMUX -eq 1 ]
            echo "fzf-tmux -d$FZF_TMUX_HEIGHT"
        else
            echo "fzf"
        end
    end

    function fzf-command-go -d "Execute current command line and filter results by fzf"
        set -q FZF_TMUX; or set FZF_TMUX 0
        set -q FZF_TMUX_HEIGHT; or set FZF_TMUX_HEIGHT 40%
        if [ $FZF_TMUX -eq 1 ]
            commandline -aj " 2>&1 | fzf-tmux -d$FZF_TMUX_HEIGHT"
        else
            commandline -aj " 2>&1 | fzf"
        end
        commandline -f execute
    end

    function open-magit -d "Open magit in emacs"
        if git rev-parse --is-inside-work-tree > /dev/null 2>&1
            emacsclient -n -eval "(+amos/workspace-new)" > /dev/null 2>&1
            emacsclient -n -eval "(magit-status $pwd)" > /dev/null 2>&1
            tmux switch-client -t emacs
            emacsclient -n -eval "(setq +amos-tmux-need-switch t)" > /dev/null 2>&1
        end
    end

    function open-ranger -d "Open ranger in emacs"
        emacsclient -n -eval "(+amos/workspace-new)" > /dev/null 2>&1
        emacsclient -n -eval "(+amos/dired-jump $pwd)" > /dev/null 2>&1
        tmux switch-client -t emacs
        emacsclient -n -eval "(setq +amos-tmux-need-switch t)" > /dev/null 2>&1
    end

    function fish_clipboard_copy
        set esc "\033]52;c;"(printf (commandline) | head -c 100000 | base64 | tr -d '\r\n')"\a"
        if count $TMUX > /dev/null
            set esc "\033Ptmux;\033"$esc"\033\\" # tmux
        end
        printf "$esc"
    end

    function pet-select
        set -l query (commandline)
        pet search --query "$query" $argv | read cmd
        commandline $cmd
    end

    function ls-commandline -d "execute exa"
        if string match -r '^ *$' (commandline) > /dev/null 2>&1
            commandline "exa"
            commandline -f execute
        else
            return
        end
    end

    function proxy-commandline -d "execute commandline using proxychains"
        if string match -r '^ *$' (commandline) > /dev/null 2>&1
            return
        else
            commandline "proxychains "(commandline)
            commandline -f execute
        end
    end

    function sudo-commandline -d "execute commandline using sudo"
        if string match -q -r '^ *$' (commandline)
            return
        else if string match -q -r '^sudo ' (commandline)
            commandline -f execute
        else if string match -q -r '^e ' (commandline)
            set -l cmd (commandline)
            commandline (string replace -r '^e ' 'E ' -- $cmd)
            commandline -f execute
        else
            commandline "sudo -Es "(commandline)
            commandline -f execute
        end
    end

    function gdb-commandline -d "execute commandline using tmuxgdb"
        if string match -r '^ *$' (commandline) > /dev/null 2>&1
            return
        else
            commandline "tmuxgdb -ex=start -args "(commandline)
            commandline -f execute
        end
    end

    function my-edit-command -d "edit command buffer or tmux buffer"
        if not string match -r '^ *$' (commandline) > /dev/null 2>&1
            # vim (tmux capture-pane -S - -E - -p | psub -f)
            edit_command_buffer
        end
    end

    function my-edit-tmux -d "edit command buffer or tmux buffer"
        if string match -r '^ *$' (commandline) > /dev/null 2>&1
            vim (tmux capture-pane -S - -E - -p | psub -f)
        end
    end

    function delete-suggestion
        commandline --with-suggestion | read -lz cmd
        builtin history delete --exact --case-sensitive $cmd
        builtin history merge
    end

    function yank-commandline
        commandline | osc52clip
    end

    bind \eD delete-suggestion
    bind \cs sudo-commandline
    bind \e` proxy-commandline # Control-Shift-S
    bind \cq gdb-commandline
    bind \em ls-commandline
    bind \cr fzf-history-token-widget
    bind \ci fzf-complete
    bind \eG open-magit
    bind \ep updir
    bind \en elvish-nav
    bind \eg fzf-jump-cd
    bind \eo myprevd
    bind \ei mynextd
    # bind \eR pet-select
    # bind \em fzf-command-go
    bind \cv fzf-select
    bind \er open-ranger
    bind \ey open-ranger
    bind \eE my-edit-command
    bind \ey yank-commandline
    function nop
    end
    bind \ee nop

    # bind \eE my-edit-tmux

    bind \e, history-token-search-forward

    # tmux

    function select-window --argument-names "n"
        tmux select-window -t $n > /dev/null 2>&1
    end

    function select-pane --argument-names "o"
        tmux select-pane $o  > /dev/null 2>&1
    end

    bind \e1 'select-window 1'
    bind \e2 'select-window 2'
    bind \e3 'select-window 3'
    bind \e4 'select-window 4'
    bind \e5 'select-window 5'
    bind \e6 'select-window 6'
    bind \e7 'select-window 7'
    bind \e8 'select-window 8'
    bind \e9 'select-window 9'
    bind \e0 'select-window 10'

    bind \eh 'select-pane -L'
    bind \ej 'select-pane -D'
    bind \ek 'select-pane -U'
    bind \el 'select-pane -R'

    bind \ev 'tmux copy-mode -u'
    bind \cx1 'tmux resize-pane -Z'
    bind \cx2 'tmux split-window -v -c "#{pane_current_path}"'
    bind \cx3 'tmux split-window -h -c "#{pane_current_path}"'
    bind \cxs 'tmux set synchronize-panes'
    bind \cx\cc 'tmux detach-client'
    bind \cxr 'tmux source-file ~/.tmux/.tmux.conf.amos \; tmux display "~/.tmux/.tmux.conf.amos sourced"'
end
