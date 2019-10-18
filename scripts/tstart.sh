#!/usr/bin/env bash

export LANG=en_US.UTF-8
export SHELL=/home/amos/gentoo/usr/local/bin/fish # for tmux
if test "$SSH_AUTH_SOCK"; then
    ln -sf $SSH_AUTH_SOCK ~/.ssh/ssh_auth_sock
fi
export SSH_AUTH_SOCK="$HOME/.ssh/ssh_auth_sock"

export PATH=/usr/bin:/usr/sbin

TMUX=/home/amos/gentoo/usr/local/bin/tmux
$TMUX -u new -d -s htop htop
if ! $TMUX list-sessions | grep -q -F emacs; then
    fuser -k /tmp/emacs.lock # sometimes emacs daemon doesn't quit
fi
$TMUX -u new -d -s emacs $SHELL -i -c startemacs
$TMUX -u new-session -A -s amos
