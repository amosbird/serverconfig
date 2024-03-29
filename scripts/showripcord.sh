#!/usr/bin/env bash

workspace=$(bspc query -D -d focused --names)
if pgrep ripcord >/dev/null; then
    id=$(head -1 /tmp/ripcord)
    if [ -z "$id" ]; then
        exit 0
    fi
    if bspc query -N -n focused | grep -q "$(bspc query -N -n "$id")"; then
        bspc node "$id".window -g hidden -f
    else
        bspc node "$id" --to-desktop "$workspace"
        bspc node "$id" -t floating
        bspc node "$id".window -g hidden=off -f
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
    rm /tmp/ripcord
    # env FONTCONFIG_FILE=~/.config/tgfonts.conf
    bash -c "ripcord &"
fi
