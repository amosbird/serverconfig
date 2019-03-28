#!/usr/bin/env bash

workspace=$(bspc query -D -d focused --names)
if pgrep obs > /dev/null
then
    id=$(head -1 /tmp/obs)
    if [ -z "$id" ]
    then
        exit 0
    fi
    if bspc query -N -n focused | grep -q "$(bspc query -N -n "$id")"
    then
        bspc node "$id" -g hidden -f
    else
        bspc node "$id" --to-desktop "$workspace"
        bspc node "$id" -t floating
        bspc node "$id" -g hidden=off -f
    fi
    read -r w h < <(xdpyinfo | grep dimensions | sed -r '/^[^0-9]*([0-9]+)x([0-9]+).*/!d;s//\1 \2/;q')
    x=$((w/8))
    y=80
    w=$((w*3/4))
    h=$((h - 140))
    xdo move -x $x -y $y "$id"
    xdo resize -w $w -h $h "$id"
    bspc node "$id" -l above
else
    rm /tmp/obs
    # env FONTCONFIG_FILE=~/.config/tgfonts.conf
    cd /usr/local/bin/64bit
    ./obs
fi
