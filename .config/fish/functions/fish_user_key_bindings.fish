function fish_user_key_bindings
  function updir
    cd ..
    commandline -f repaint
    eval (direnv export fish);
  end

  function myprevd
    prevd ^&1 > /dev/null
    commandline -f repaint
    eval (direnv export fish);
  end

  function mynextd
    nextd ^&1 > /dev/null
    commandline -f repaint
    eval (direnv export fish);
  end

  function fzf-jump-cd -d "Change directory"
    set -q FZF_ALT_C_COMMAND; or set -l FZF_ALT_C_COMMAND "command jump top"
    set -q FZF_TMUX_HEIGHT; or set FZF_TMUX_HEIGHT 40%
    begin
      set -lx FZF_DEFAULT_OPTS "+s --height $FZF_TMUX_HEIGHT --reverse $FZF_DEFAULT_OPTS $FZF_ALT_C_OPTS"
      eval "$FZF_ALT_C_COMMAND | "(__fzfcmd)" +m" | read -l result
      if string match -r '^ *$' (commandline) > /dev/null ^&1
        [ "$result" ]
        and if not cd $result
          jump clean
        end
      else
        [ "$result" ]
        and commandline -i $result
      end
    end
    commandline -f repaint
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

  function fzf-cd-widget -d "Change directory"
    if type -q bfs
      set cmd bfs
    else
      set cmd find
    end
    # set -q FZF_ALT_C_COMMAND; or set -l FZF_ALT_C_COMMAND "command $cmd -type d"
    set -q FZF_ALT_C_COMMAND; or set -l FZF_ALT_C_COMMAND "command find ./ -mindepth 1 -maxdepth 1 -type d"
    set -q FZF_TMUX_HEIGHT; or set FZF_TMUX_HEIGHT 40%
    begin
      set -lx FZF_DEFAULT_OPTS "--height $FZF_TMUX_HEIGHT --reverse $FZF_DEFAULT_OPTS $FZF_ALT_C_OPTS"
      eval "$FZF_ALT_C_COMMAND | "(__fzfcmd)" +m" | read -l result
      [ "$result" ]; and cd $result
    end
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
      commandline -aj " ^&1 | fzf-tmux -d$FZF_TMUX_HEIGHT"
    else
      commandline -aj " ^&1 | fzf"
    end
    commandline -f execute
  end

  function last-sudo -d "Execute last command using sudo if current commandline is blank"
    if string match -r '^ *$' (commandline) > /dev/null ^&1
      commandline -a "sudo $history[1]"
      commandline -f execute
    end
  end

  function open-magit -d "Open magit in emacs"
    if emacsclient -n -eval "(magit-status $pwd)" > /dev/null ^&1
      if test -z $T450s
        tmux switch-client -t emacs
      else
        i3-msg '[instance="^urxvt_scratchpad"]' move to scratchpad > /dev/null ^&1
        i3-msg  workspace  > /dev/null ^&1
      end
    end
  end

  function open-ranger -d "Open ranger in emacs"
    if emacsclient -n -eval "(ranger $pwd)" > /dev/null ^&1
      if test -z $T450s
        tmux switch-client -t emacs
      else
        i3-msg '[instance="^urxvt_scratchpad"]' move to scratchpad > /dev/null ^&1
        i3-msg workspace  > /dev/null ^&1
      end
    end
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

  function ls-commandline -d "execute ls"
    if string match -r '^ *$' (commandline) > /dev/null ^&1
      commandline "ls"
      commandline -f execute
    else
      return
    end
  end

  function proxy-commandline -d "execute commandline using proxychains"
    if string match -r '^ *$' (commandline) > /dev/null ^&1
      return
    else
      commandline "proxychains "(commandline)
      commandline -f execute
    end
  end

  function sudo-commandline -d "execute commandline using sudo"
    if string match -r '^ *$' (commandline) > /dev/null ^&1
      return
    else
      commandline "sudo "(commandline)
      commandline -f execute
    end
  end

  function my-edit-command -d "edit command buffer or tmux buffer"
    if string match -r '^ *$' (commandline) > /dev/null ^&1
      vim (tmux capture-pane -S - -E - -p | psub -f)
    else
      edit_command_buffer
    end
  end

  bind \cs sudo-commandline
  bind \e` proxy-commandline
  bind \em ls-commandline
  bind \cr fzf-history-token-widget
  bind \ci fzf-complete
  bind \cg open-magit
  bind \ep updir
  # bind \en fzf-cd-widget
  bind \en elvish-nav
  bind \eg fzf-jump-cd
  bind \eo myprevd
  bind \ei mynextd
  bind \eu pet-select
  # bind \em fzf-command-go
  bind \cv fzf-select
  bind \er open-ranger
  bind \ee my-edit-command

  # tmux

  function select-window --argument-names "n"
    tmux select-window -t $n > /dev/null ^&1
  end

  function select-pane --argument-names "o"
    tmux select-pane $o  > /dev/null ^&1
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
