#!/usr/bin/env bash

export LANG=en_US.UTF-8
export SHELL=/home/amos/gentoo/usr/local/bin/fish # for tmux
export HOME=/home/amos
export TERM=xterm-tmux-24bits
if test "$SSH_AUTH_SOCK"; then
    ln -sf $SSH_AUTH_SOCK ~/.ssh/ssh_auth_sock
fi
export SSH_AUTH_SOCK="$HOME/.ssh/ssh_auth_sock"

if [ -z "$1" ]; then
    export PATH=/home/amos/gentoo/usr/local/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin
else
    export PATH=$PATH:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin
fi

tmux -u new -d -s htop htop
if ! tmux list-sessions | grep -q -F emacs; then
    fuser -k /tmp/emacs.lock &>/dev/null # sometimes emacs daemon doesn't quit
fi
tmux -u new -d -s emacs $SHELL -i -c startemacs
tmux -u new-session -A -s amos
