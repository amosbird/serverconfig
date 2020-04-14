#!/usr/bin/env bash

workspace=$(bspc query -D -d focused --names)
if pgrep -f TIM &>/dev/null; then
    id=$(tail -1 /tmp/tim)
    if [ -z "$id" ]; then
        exit 0
    fi
    wid=$(bspc query -N -n "$id")
    if [ -z "$wid" ]; then
        exit 0
    elif bspc query -N -n focused | grep -q "$wid"; then
        bspc node "$id" -g hidden -f
        exit 0
    else
        bspc node "$id" --to-desktop "$workspace"
        bspc node "$id" -t floating
        bspc node "$id" -g hidden=off -f
    fi
    wh=($(xrandr --current | perl -ne 'if (/primary/) {@x=split; $x[3] =~ /(\d+)x(\d+)/; print $1." ".$2}'))
    w=${wh[0]}
    h=${wh[1]}
    x=$((w / 8))
    y=80
    w=$((w * 3 / 4))
    h=$((h - 140))
    xdo move -x $x -y $y "$id"
    xdo resize -w $w -h $h "$id"
    bspc node "$id" -l above
else
    rm /tmp/tim
    # env FONTCONFIG_FILE=~/.config/tgfonts.conf
    bash -c "tim.sh &> /dev/null"
fi
