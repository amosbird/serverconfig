#!/usr/bin/env bash

if [[ $1 -eq 1 ]]; then # new session
    case "$2" in
    emacs)
        tmux source $HOME/.tmux/.tmux.conf.emacs
        ;;
    htop)
        tmux source $HOME/.tmux/.tmux.conf.htop
        ;;
    amos)
        tmux source $HOME/.tmux/.tmux.conf.amos
        ;;
    esac
elif [[ $1 -eq 2 ]]; then # session change
    if [[ $2 == emacs ]]; then
        tmux send f12
    elif [[ $2 == htop ]]; then
        tmux run-shell "$HOME/scripts/tmuxsuspend USR2"
    else
        $HOME/scripts/setcursor.sh $(tmux display -p "#{pane_tty}")
    fi
elif [[ $1 -eq 3 ]]; then # detach
    if [[ $2 == htop ]]; then
        tmux run-shell "$HOME/scripts/tmuxsuspend USR1"
    fi
elif [[ $1 -eq 4 ]]; then # new window
    tmux source "$HOME/.tmux/.tmux.conf.amos"
fi
