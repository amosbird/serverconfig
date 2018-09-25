#!/usr/bin/env bash

case "$1" in
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
