#!/usr/bin/env bash

wh=($(xdpyinfo | grep dimensions | sed -r 's/^[^0-9]*([0-9]+x[0-9]+).*$/\1/' | awk -Fx '{print $1" "$2}'))
w=${wh[0]}
h=${wh[1]}

function reset_tray {
    wid=$(cat /tmp/stalonetray)
    x=$((w/2 - 70))
    y=300
    xdo move -x $x -y $y $wid
}

if (( $# == 0 ))
then
    reset_tray
    feh --bg-scale ~/git/serverconfig/black.jpg
else
    $1
fi
