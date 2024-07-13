#!/usr/bin/env bash

workspace=$(bspc query -D -d focused --names)
if pgrep -f /opt/ioa/bin/iOALinux &>/dev/null; then
    while read -r wid
    do
        xprop -id "$wid" | grep -E -q "window state: (Normal|Iconic)" && found=1 && break
    done < <(xdo id -N iOALinux.bin -n iOALinux.bin)
    if [ -z "$found" ]; then
        /opt/ioa/bin/iOALinux &
    elif bspc query -N -n focused | grep -q "$wid"; then
        bspc node older.!hidden -f
        bspc node "$wid".window -g hidden
        exit 0
    else
        bspc node "$wid" -t floating
        bspc node "$wid".window -g hidden=off
        bspc node "$wid" --to-desktop "$workspace"
    fi
    wh=($(xrandr --current | perl -ne 'if (/primary/) {@x=split; $x[3] =~ /(\d+)x(\d+)/; print $1." ".$2}'))
    w=${wh[0]}
    h=${wh[1]}
    x=$((w / 4))
    y=200
    w=$((w * 19 / 40))
    h=$((h * 11 / 20))
    xdo move -x $x -y $y "$wid"
    xdo resize -w $w -h $h "$wid"
    bspc node "$wid".window -f
    bspc node "$wid" -l above
else
    rm /tmp/ioa
    # env FONTCONFIG_FILE=~/.config/tgfonts.conf
    coproc _ (/opt/ioa/bin/iOALinux &> /dev/null)
fi
