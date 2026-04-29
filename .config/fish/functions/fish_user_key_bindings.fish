function fish_user_key_bindings
    function updir
        cd ..
        commandline -f repaint
        if count $TMUX > /dev/null
            tmux refresh-client -S
            tmux refresh-client -S
        end
        eval (direnv export fish);
    end

    function myprevd
        prevd > /dev/null 2>&1
        commandline -f repaint
        if count $TMUX > /dev/null
            tmux refresh-client -S
            tmux refresh-client -S
        end
        eval (direnv export fish);
    end

    function mynextd
        nextd > /dev/null 2>&1
        commandline -f repaint
        if count $TMUX > /dev/null
            tmux refresh-client -S
            tmux refresh-client -S
        end
        eval (direnv export fish);
    end

    function fzf-jump-cd -d "Change directory"
        set -q FZF_TMUX_HEIGHT; or set FZF_TMUX_HEIGHT 40%
        begin
            set -lx FZF_DEFAULT_OPTS "--expect ctrl-space,alt-enter --height $FZF_TMUX_HEIGHT --reverse $FZF_DEFAULT_OPTS"
            set -l result (jump-top | fzf +m)
            set -l dir (string trim -- "$result[2]")
            if test -n "$dir"
                if test -z "$result[1]"
                    set updated_path (string replace -r "^~" "$HOME" -- "$dir")
                    if not cd "$updated_path"
                        jump clean
                    end
                else
                    commandline -i -- "$dir/"
                end
            end
        end
        commandline -f repaint
        if count $TMUX > /dev/null
            tmux refresh-client -S
            tmux refresh-client -S
        end
        eval (direnv export fish);
    end

    function fzf-select -d 'fzf commandline job and print unescaped selection back to commandline'
        set -l cmd (commandline -j)
        set -q FZF_TMUX_HEIGHT; or set FZF_TMUX_HEIGHT 40%
        [ "$cmd" ]; or return
        set -l result (eval $cmd | fzf -m --height $FZF_TMUX_HEIGHT --reverse --tiebreak=index --select-1 --exit-0 | string join ' ')
        [ "$result" ]; and commandline -j -- $result
        commandline -f repaint
    end

    function fzf-history-token-widget -d "Show command history"
        set -q FZF_TMUX_HEIGHT; or set FZF_TMUX_HEIGHT 40%
        set str (commandline -jc)
        set tok (commandline -tc)
        begin
            set -lx FZF_DEFAULT_OPTS "--reverse --height $FZF_TMUX_HEIGHT $FZF_DEFAULT_OPTS +s --tiebreak=index --bind=ctrl-r:toggle-sort $FZF_CTRL_R_OPTS +m"
            if [ $str = $tok ]
                _atuin_search
            else
                set -l list
                for his in $history
                    echo $his | read --tokenize --array tokens
                    set -a list $tokens[-1..1]
                    if test (count $list) -gt 256
                        break
                    end
                end
                string join0 -- $list | fzf --read0 -q $tok | read -l result
                and commandline -tr -- $result
                commandline -f repaint
            end
        end
    end

    function open-magit -d "Open magit in emacs"
        if git rev-parse --is-inside-work-tree > /dev/null 2>&1
            set -l sn (tmux display-message -p '#S')
            set -l EMACS
            if [ $sn = "gui" ]
                emacspopup "(cd \"$PWD\") (magit-status)"
            else
                set EMACS emacsclient -n -q -u -e
                kitten @ --to unix:/tmp/kitty_sock focus-window --match id:2
                $EMACS "(progn (+amos/workspace-new) (cd \"$PWD\") (magit-status))"
            end
        end
    end

    function open-ranger -d "Open dired in emacs"
        set -l sn (tmux display-message -p '#S')
        set -l EMACS
        if [ $sn = "gui" ]
            emacspopup "(cd \"$PWD\") (+amos/dired-jump)"
        else
            kitten @ --to unix:/tmp/kitty_sock focus-window --match id:2
            set EMACS emacsclient -n -q -u -e
            $EMACS "(progn (+amos/workspace-new) (cd \"$PWD\") (+amos/dired-jump))"
        end
    end

    function fish_clipboard_copy
        set esc "\033]52;c;"(printf (commandline) | head -c 100000 | base64 | tr -d '\r\n')"\a"
        if count $TMUX > /dev/null
            set esc "\033Ptmux;\033"$esc"\033\\"
        end
        printf "$esc"
    end

    function ls-commandline -d "execute eza"
        if string match -r '^ *$' (commandline) > /dev/null 2>&1
            commandline "eza"
            commandline -f execute
        end
    end

    function proxy-commandline -d "execute commandline using proxychains"
        if not string match -r '^ *$' (commandline) > /dev/null 2>&1
            commandline "proxychains "(commandline)
            commandline -f execute
        end
    end

    function sudo-commandline -d "execute commandline using sudo"
        if string match -q -r '^ *$' -- (commandline)
            return
        else if string match -q -r '^sudo ' -- (commandline)
            commandline -f execute
        else if string match -q -r '^e ' -- (commandline)
            set -l cmd (commandline)
            commandline (string replace -r '^e ' 'E ' -- $cmd)
            commandline -f execute
        else
            commandline "sudo "(commandline)
            commandline -f execute
        end
    end

    function gdb-commandline -d "execute commandline using tmuxgdb"
        if not string match -r '^ *$' (commandline) > /dev/null 2>&1
            commandline "tmuxgdb "(commandline)
            commandline -f execute
        end
    end

    function my-edit-command -d "edit command buffer"
        if not string match -r '^ *$' (commandline) > /dev/null 2>&1
            edit_command_buffer
        end
    end

    function yank-commandline
        commandline | osc52clip
    end

    function insert-last-arg
        set -l a (commandline -co)[-1]
        test -n "$a"
        and commandline -t -- $a
    end

    function insert-last-line
        if test -n "$TMUX"
            set -l a (string escape -n -- (string trim -- (tmux capture-pane -p | rg -v '^$|❯|^\\[' | tail -1)))
        else
            set -l a (string escape -n -- (string trim -- (kitty @ --to unix:/tmp/kitty-mux-socket get-text --extent screen | rg -v '^$|❯|^\\[' | tail -1)))
        end
        test -n "$a"
        and commandline -t -- $a
    end

    function start_bracketed_paste
        set -g __fish_last_bind_mode $fish_bind_mode
        string match -q 'single*' (__fish_tokenizer_state -- (commandline -ct | string collect))
        and set -g __fish_paste_quoted 1
        set -l cmdline (commandline | string collect -N)
        set -g __fish_amos_cursor (commandline -C)
        set -g __fish_amos_cmd_pre (string sub -l $__fish_amos_cursor -- $cmdline | head -c -1 | string collect -N)

        if test -z $__fish_amos_cmd_pre
            set -g __fish_amos_cmd_pre ""
        end

        set -g __fish_amos_cmd_post (string sub -s (math $__fish_amos_cursor + 1) -- $cmdline | string collect)

        commandline -r ""
    end

    function stop_bracketed_paste
        set fish_bind_mode $__fish_last_bind_mode
        set -e __fish_paste_quoted
        set -l cmdline (commandline | string collect)
        commandline -r -- $__fish_amos_cmd_pre$cmdline$__fish_amos_cmd_post
        commandline -C (math $__fish_amos_cursor + (string length -- "$cmdline"))
        set -e __fish_amos_cmd
        set -e __fish_amos_cursor
        commandline -f force-repaint
    end

    function _atuin_search
        set -l ATUIN_H "$(ATUIN_SHELL_FISH=t ATUIN_LOG=error atuin search $argv -i -- (commandline -b) 3>&1 1>&2 2>&3)"

        if test -n "$ATUIN_H"
            if string match --quiet '__atuin_accept__:*' "$ATUIN_H"
                set -l ATUIN_HIST "$(string replace "__atuin_accept__:" "" -- "$ATUIN_H")"
                commandline -r "$ATUIN_HIST"
                commandline -f repaint
                commandline -f execute
                return
            else
                commandline -r "$ATUIN_H"
            end
        end

        commandline -f repaint
    end

    function _atuin_bind_up
        if commandline --search-mode; or commandline --paging-mode
            up-or-search
            return
        end

        set -l lineno (commandline --line)

        switch $lineno
            case 1
                _atuin_search --shell-up-key-binding
            case '*'
                up-or-search
        end
    end

    for mode in (bind --list-modes | string match -v paste)
        bind --preset -M $mode -m paste \e\[200~ 'start_bracketed_paste'
    end
    bind --preset -M paste \e\[201~ 'stop_bracketed_paste'

    bind \cs sudo-commandline
    bind \e` proxy-commandline
    bind \cq gdb-commandline
    bind \em ls-commandline
    bind tab __fzf_complete
    bind \eG open-magit
    bind \ep updir
    bind \en br
    bind \eg fzf-jump-cd
    bind \eo myprevd
    bind \ei mynextd
    bind \cv fzf-select
    bind \er open-ranger
    bind \ey yank-commandline
    bind \eE my-edit-command
    bind \ee nop
    bind \ev nop
    bind \e, history-token-search-forward
    bind alt-shift-. 'insert-last-arg'
    bind alt-shift-comma 'insert-last-line'
    bind ctrl-r fzf-history-token-widget
    bind alt-backspace backward-kill-path-component
end
