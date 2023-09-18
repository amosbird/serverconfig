#!/usr/bin/env bash

export LANG=en_US.UTF-8
export SHELL=/tmp/gentoo/bin/fish # for tmux
export TERM=xterm-kitty
export TMPDIR=/tmp/gentoo/tmp
export TMUX=$TMPDIR/tmux-amos
export SSH_AUTH_SOCK="$TMPDIR/ssh_auth_sock"
export NPROC=$(nproc)
export MANPATH=  # Rely on man_db.conf instead

if test -s /tmp/gentoo/etc/hostname; then
    export HOSTNAME=$(cat /tmp/gentoo/etc/hostname)
fi

case $1 in
android)
    export PATH=$PATH:/system/bin:/system/xbin:/system/sbin:/data/adb/modules/ssh/usr/bin
    ;;
local)
    # export PATH=$HOME/gentoo/usr/local/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin
    ;;
prefix)
    export PATH=$PATH:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin
    GENTOO_DIR=$(readlink /tmp/gentoo)
    if [ -d "$GENTOO_DIR" ]
    then
        "$GENTOO_DIR"/home/amos/scripts/keepgentoo.py "$GENTOO_DIR" &
        sleep 0.2
    else
        echo "gentoo dir suddenly disappear"
        exit 1
    fi
    ;;
*)
    echo "tstart.sh android|local|prefix"
    exit 1
esac

tmux -u new -d -s htop "exec starthtop"
if ! tmux list-sessions | grep -q -F emacs; then
    pkill -9 -F "$TMPDIR/emacs-server"
fi
tmux -u new -d -s emacs "exec startemacs"
tmux -u new-session -A -s amos
