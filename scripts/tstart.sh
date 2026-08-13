#!/usr/bin/env bash

if [[ ${GUI:-} != t ]]; then
    # shellcheck source=/dev/null
    source "$HOME/.config/environment.sh"
fi

mode=${1:-}
SSH_SERVER=${2:-${SSH_SERVER:-}}
SSH_MASTER_CTRL=${3:-${SSH_MASTER_CTRL:-}}
apps=(emacs htop crush abush)

case "$mode" in
android)
    export PATH="$PATH:/system/bin:/system/xbin:/system/sbin:/data/adb/modules/ssh/usr/bin"
    exec tmux -u new-session -A -s amos
    ;;
local)
    for app in "${apps[@]}"; do
        kitten @ launch --allow-remote-control --keep-focus "$0" "$app"
    done
    ;;
remote)
    if [[ -z $SSH_SERVER || -z $SSH_MASTER_CTRL ]]; then
        echo "tstart.sh remote requires SSH_SERVER and SSH_MASTER_CTRL" >&2
        exit 1
    fi
    for app in "${apps[@]}"; do
        kitten @ launch --allow-remote-control --keep-focus ssh -S "$SSH_MASTER_CTRL" -tt "$SSH_SERVER" "$0" "$app"
    done
    ;;
emacs)
    exec startemacs
    ;;
htop)
    exec starthtop
    ;;
lvim)
    exec startlvim
    ;;
crush)
    exec startcrush-tmux
    ;;
abush)
    exec startabush
    ;;
*)
    echo "usage: tstart.sh android|local|remote|emacs|htop|lvim|crush|abush [ssh_server ssh_master_ctrl]" >&2
    exit 1
    ;;
esac

export TMUX=/tmp/tmux-amos
tmux -u new-session -A -s amos
