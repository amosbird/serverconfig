#!/usr/bin/env bash

if ! xwininfo -name "popup" >/dev/null 2>&1; then
    filedialog
    id=$(cat /tmp/popup)
else
    workspace=$(bspc query -D -d focused --names)
    id=$(cat /tmp/popup)
    if [[ -z "$id" ]]; then
        exit 0
    fi

    if bspc query -N -n focused | grep -q "$(bspc query -N -n "$id")"; then
        bspc node "$id" -g hidden -f
    else
        bspc node "$id" --to-desktop "$workspace"
        bspc node "$id" -t floating
        bspc node "$id" -g hidden=off -f
    fi
fi

wh=($(xdpyinfo | grep dimensions | sed -r '/^[^0-9]*([0-9]+)x([0-9]+).*/!d;s//\1 \2/;q'))
w=${wh[0]}
h=${wh[1]}
x=$((w / 8))
y=50
w=$((w * 3 / 4))
h=$((h - 100))
xdo move -x $x -y $y "$id"
xdo resize -w $w -h $h "$id"
bspc node "$id" -l above
