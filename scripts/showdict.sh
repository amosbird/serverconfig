#!/usr/bin/env bash

if ! pgrep -f  'sdcv' > /dev/null
then
    urxvt -T stardict -e dict.sh &
    sleep 0.5
fi

id=$(cat /tmp/stardict)
if [[ -z "$id" ]]
then
    exit 0
fi

bspc node "$id" -g sticky=on -g hidden -f
wh=($(xdpyinfo | grep dimensions | sed -r '/^[^0-9]*([0-9]+)x([0-9]+).*/!d;s//\1 \2/;q'))
w=${wh[0]}
h=${wh[1]}
x=$((w/4))
y=30
w=$((w/2))
h=$((h - 60))
xdo move -x $x -y $y "$id"
xdo resize -w $w -h $h "$id"
