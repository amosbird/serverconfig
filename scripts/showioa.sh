#!/usr/bin/env bash

workspace=$(bspc query -D -d focused --names)
if pgrep -f /usr/lib/iOA/bin/iOALinux &>/dev/null; then
    while read -r wid
    do
        xprop -id "$wid" | grep -E -q "window state: (Normal|Iconic)" && found=1 && break
    done < <(xdo id -N iOALinux -n iOALinux)
    if [ -z "$found" ]; then
        /usr/lib/iOA/bin/iOALinux &
    elif bspc query -N -n focused | grep -q "$wid"; then
        bspc node "$wid" -g hidden -f
        exit 0
    else
        bspc node "$wid" --to-desktop "$workspace"
        bspc node "$wid" -t floating
        bspc node "$wid" -g hidden=off -f
    fi
    wh=($(xrandr --current | perl -ne 'if (/primary/) {@x=split; $x[3] =~ /(\d+)x(\d+)/; print $1." ".$2}'))
    w=${wh[0]}
    h=${wh[1]}
    x=$((w / 4))
    y=400
    w=1840
    h=1200
    xdo move -x $x -y $y "$wid"
    xdo resize -w $w -h $h "$wid"
    bspc node "$wid" -l above
else
    rm /tmp/ioa
    # env FONTCONFIG_FILE=~/.config/tgfonts.conf
    bash -c "/usr/lib/iOA/bin/iOALinux &> /dev/null"
fi
