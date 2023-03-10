#!/usr/bin/env bash

if ! xwininfo -name "nnnpopup" >/dev/null 2>&1; then
    filedialog
    id=$(cat /tmp/nnnpopup)
else
    workspace=$(bspc query -D -d focused --names)
    id=$(cat /tmp/nnnpopup)
    if [[ -z $id ]]; then
        exit 0
    fi

    if bspc query -N -n focused | grep -q "$(bspc query -N -n "$id")"; then
        bspc node "$id".window -g hidden -f
    else
        bspc node "$id" --to-desktop "$workspace"
        bspc node "$id" -t floating
        bspc node "$id".window -g hidden=off -f
    fi
fi

wh=($(xrandr --current | perl -ne 'if (/primary/) {@x=split; $x[3] =~ /(\d+)x(\d+)/; print $1." ".$2}'))
w=${wh[0]}
h=${wh[1]}
x=$((w / 4))
y=50
w=$((w * 3 / 6))
h=$((h - 100))
xdo move -x "$x" -y "$y" "$id"
xdo resize -w "$w" -h "$h" "$id"
bspc node "$id" -l above
