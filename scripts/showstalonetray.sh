#!/usr/bin/env bash

if ! xwininfo -name "stalonetray" >/dev/null 2>&1; then
    rm /tmp/stalonetray
    case $(hostname) in
        abt480)
            stalonetray --icon-size=48 --kludges=force_icons_size &> /tmp/stalonetray.log &
            ;;
        *)
            stalonetray --icon-size=96 --kludges=force_icons_size &> /tmp/stalonetray.log &
            ;;
    esac
else
    workspace=$(bspc query -D -d focused --names)
    id=$(cat /tmp/stalonetray)
    if [[ -z $id ]]; then
        exit 0
    fi

    if bspc query -N -n focused | grep -q "$(bspc query -N -n "$id")"; then
        bspc node "$id" -g hidden -f
        exit 0
    else
        bspc node "$id" --to-desktop "$workspace"
        bspc node "$id" -t floating
        bspc node "$id" -g hidden=off -f
    fi
fi
