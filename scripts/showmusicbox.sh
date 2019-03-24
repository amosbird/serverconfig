#!/usr/bin/env bash

workspace=$(bspc query -D -d focused --names)
if pgrep mpv > /dev/null
then
    id=$(head -1 /tmp/mpv)
    if [ -z "$id" ]
    then
        exit 0
    fi
    if bspc query -N -d focused | grep -q "$(bspc query -N -n "$id")"
    then
        bspc node "$id" -g hidden -f
    else
        bspc node "$id" --to-desktop "$workspace"
        bspc node "$id" -t floating
        bspc node "$id" -g hidden=off -f
    fi
    read -r w h < <(xdpyinfo | grep dimensions | sed -r '/^[^0-9]*([0-9]+)x([0-9]+).*/!d;s//\1 \2/;q')
    read -r x y < <(xwininfo -id "$id" | perl -ne 'print $1, " " if /(?:Width|Height): (.*)/')
    x=$(( (w - x) / 2))
    y=$(( (h - y) / 2))
    xdo move -x $x -y $y "$id"
    bspc node "$id" -l above -g center
fi
