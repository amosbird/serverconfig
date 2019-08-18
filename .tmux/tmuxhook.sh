#!/usr/bin/env bash

if [[ $1 -eq 1 ]]; then
    case "$2" in
    emacs)
        tmux source ~/.tmux/.tmux.conf.emacs
        ;;
    htop)
        tmux source ~/.tmux/.tmux.conf.htop
        ;;
    amos)
        tmux source ~/.tmux/.tmux.conf.amos
        ;;
    gui)
        tmux source ~/.tmux/.tmux.conf.gui
        ;;
    esac
elif [[ $1 -eq 2 ]]; then
    if [[ $2 == emacs ]]; then
        tmux send f12
    elif [[ $2 == htop ]]; then
        tmux send -X cancel
    else
        $HOME/scripts/setcursor.sh $(tmux display -p "#{pane_tty}")
    fi
elif [[ $1 -eq 3 ]]; then
    if [[ $2 == htop ]]; then
        tmux copy-mode
    fi
fi
