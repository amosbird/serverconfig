#!/usr/bin/env bash

export LANG=en_US.UTF-8
export SHELL=$HOME/gentoo/usr/local/bin/fish # for tmux
export TERM=xterm-tmux-24bits
if test "$SSH_AUTH_SOCK"; then
    ln -sf $SSH_AUTH_SOCK ~/.ssh/ssh_auth_sock
fi
export SSH_AUTH_SOCK="$HOME/.ssh/ssh_auth_sock"

case $1 in
android)
    export PATH=$PATH:/system/bin:/system/xbin:/system/sbin:/data/adb/modules/ssh/usr/bin
    ;;
local)
    # export PATH=$HOME/gentoo/usr/local/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin
    ;;
prefix)
    export PATH=$PATH:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin
    ;;
*)
    echo "tstart.sh android|local|prefix"
    exit 1
esac

tmux -u new -d -s htop "exec starthtop"
if ! tmux list-sessions | grep -q -F emacs; then
    fuser -k /tmp/emacs.lock &>/dev/null # sometimes emacs daemon doesn't quit
fi
tmux -u new -d -s emacs "exec startemacs"
tmux -u new-session -A -s amos
