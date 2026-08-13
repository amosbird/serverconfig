#!/usr/bin/env bash

has_active_x11_session() {
    local session type active

    command -v loginctl >/dev/null 2>&1 || return 1
    while read -r session _ user _; do
        [[ $user == "${USER:-$(id -un)}" ]] || continue
        type=$(loginctl show-session "$session" --property=Type --value 2>/dev/null)
        active=$(loginctl show-session "$session" --property=Active --value 2>/dev/null)
        [[ $type == x11 && $active == yes ]] && return 0
    done < <(loginctl list-sessions --no-legend --no-pager 2>/dev/null)
    return 1
}

if [[ ${GUI:-} != t ]]; then
    # shellcheck source=/dev/null
    source "$HOME/.config/environment.sh"
    if has_active_x11_session; then
        # shellcheck source=/dev/null
        source "$HOME/.config/path.sh" gui
    else
        # shellcheck source=/dev/null
        source "$HOME/.config/path.sh" remote
    fi
fi

mode=${1:-}
SSH_SERVER=${2:-${SSH_SERVER:-}}
SSH_MASTER_CTRL=${3:-${SSH_MASTER_CTRL:-}}
apps=(emacs htop crush abush)

case "$mode" in
android)
    # shellcheck source=/dev/null
    source "$HOME/.config/path.sh" android
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
