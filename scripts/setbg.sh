#!/usr/bin/env bash

wh=($(xdpyinfo | grep dimensions | sed -r 's/^[^0-9]*([0-9]+x[0-9]+).*$/\1/' | awk -Fx '{print $1" "$2}'))
w=${wh[0]}
h=${wh[1]}

function reset_tray() {
    return
    wid=$(cat /tmp/stalonetray)
    x=$((w / 2 - 70))
    y=260
    bspc node "$wid" --to-desktop n
    xdo move -x "$x" -y "$y" "$wid"
}

function reset_conky() {
    wid=$(cat /tmp/conky)
    bspc node "$wid" --to-desktop n
    xdo move -x 0 -y 0 "$wid"
}

if (($#)); then
    $1
else
    reset_tray
    reset_conky
    # bspc monitor -d w e d f v n i o p c h
    feh --bg-scale ~/git/serverconfig/black.jpg
fi
