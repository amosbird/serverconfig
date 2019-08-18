#!/usr/bin/env bash

workspace=$(bspc query -D -d focused --names)
if pgrep mpv >/dev/null; then
    id=$(head -1 /tmp/mpv)
    if [[ -z $id ]]; then
        exit 0
    fi
    if bspc query -N -n focused | grep -q "$(bspc query -N -n "$id")"; then
        bspc node "$id" -g hidden -f
    else
        bspc node "$id" --to-desktop "$workspace"
        bspc node "$id" -t floating
        bspc node "$id" -g hidden=off -f
    fi
    wh=($(xrandr --current | perl -ne 'if (/primary/) {@x=split; $x[3] =~ /(\d+)x(\d+)/; print $1." ".$2}'))
    w=${wh[0]}
    h=${wh[1]}
    read -r x y < <(xwininfo -id "$id" | perl -ne 'print $1, " " if /(?:Width|Height): (.*)/')
    x=$(((w - x) / 2))
    y=$(((h - y) / 2))
    xdo move -x "$x" -y "$y" "$id"
    bspc node "$id" -l above -g center
fi
