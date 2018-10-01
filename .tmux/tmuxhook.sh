#!/usr/bin/env bash

if [[ $1 -eq 1 ]]
then
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
elif [[ $1 -eq 2 ]]
then
    if [[ $2 = emacs ]]
    then
        tmux send f12
    else
        /home/amos/scripts/setcursor.sh $(tmux display -p "#{pane_tty}")
    fi
fi
