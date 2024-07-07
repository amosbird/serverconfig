#!/usr/bin/env bash

export LANG=en_US.UTF-8
export SHELL=/tmp/gentoo/bin/fish # for tmux command
export TERM=xterm-kitty
# export TMPDIR=/tmp/gentoo/tmp
export TMPDIR=/tmp
export TMUX=$TMPDIR/tmux-amos
export SSH_AUTH_SOCK=$TMPDIR/ssh_auth_sock
export KITTY_LISTEN_ON=unix:$TMPDIR/kitty_sock
export NPROC=$(nproc)
export MANPATH=  # Rely on man_db.conf instead
export LSP_USE_PLISTS=true

if test -s /tmp/gentoo/etc/hostname; then
    export HOSTNAME=$(cat /tmp/gentoo/etc/hostname)
fi

case $1 in
android)
    export PATH=$PATH:/system/bin:/system/xbin:/system/sbin:/data/adb/modules/ssh/usr/bin
    ;;
local)
    # export PATH=$HOME/gentoo/usr/local/bin:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin
    kitten @ launch --allow-remote-control --keep-focus fish -c "tstart.sh emacs"
    ;;
prefix)
    export PATH=$PATH:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin
    kitten @ launch --allow-remote-control --keep-focus ssh -S $SSH_MASTER_CTRL -tt $SSH_SERVER $LOGIN_PATH emacs
    kitten @ launch --allow-remote-control --keep-focus ssh -S $SSH_MASTER_CTRL -tt $SSH_SERVER $LOGIN_PATH lvim
    ;;
emacs)
    export PATH=$PATH:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin
    fish -c startemacs
    exit 0
    ;;
lvim)
    export PATH=$PATH:/usr/local/bin:/usr/local/sbin:/usr/bin:/usr/sbin:/bin:/sbin
    fish -c startlvim
    exit 0
    ;;
*)
    echo "tstart.sh android|local|prefix"
    exit 1
esac

tmux -u new -d -s htop "exec starthtop"
# if ! tmux list-sessions | grep -q -F emacs; then
#     pkill -9 -F "$TMPDIR/emacs-server"
# fi
# tmux -u new -d -s emacs "exec startemacs"
tmux -u new-session -A -s amos
