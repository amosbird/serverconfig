#!/usr/bin/env bash

if pgrep -f  'sdcv'
then
    :
else
    sakura -t stardict -e dict.sh &
    sleep 0.5
fi

id=$(cat /tmp/stardict)
bspc node $id -g hidden -f
wh=($(xdpyinfo | grep dimensions | sed -r 's/^[^0-9]*([0-9]+x[0-9]+).*$/\1/' | awk -Fx '{print $1" "$2}'))
w=${wh[0]}
h=${wh[1]}
x=$((w/4))
y=30
w=$((w/2))
h=$((h - 60))
xdo move -x $x -y $y $id
xdo resize -w $w -h $h $id
