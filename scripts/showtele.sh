#!/usr/bin/env bash

workspace=$(bspc query -D -d focused --names)
if pgrep -f telegram > /dev/null
then
    id=$(cat /tmp/telegram)
    if [ -z "$id" ]
    then
        exit 0
    fi
    if bspc query -N -d focused | grep -q "$(bspc query -N -n "$id")"
    then
        bspc node "$id" -g hidden -f
    else
        bspc node "$id" --to-desktop "$workspace"
        bspc node "$id" -g hidden=off -f
        bspc node "$id" -t floating
        bspc node "$id" -l above
    fi
    wh=($(xdpyinfo | grep dimensions | sed -r '/^[^0-9]*([0-9]+)x([0-9]+).*/!d;s//\1 \2/;q'))
    w=${wh[0]}
    h=${wh[1]}
    x=$((w/4 - 80))
    y=80
    w=$((w/2 + 80))
    h=$((h - 140))
    xdo move -x $x -y $y "$id"
    xdo resize -w $w -h $h "$id"
else
    env FONTCONFIG_FILE=~/.config/tgfonts.conf telegram-desktop
fi
