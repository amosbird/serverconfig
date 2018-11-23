#!/usr/bin/env bash

workspace=$(bspc query -D -d focused --names)

id=$(cat /tmp/scrcpy)
if [[ -z "$id" ]]
then
    echo z | nc -U /tmp/scrcpy.socket
    exit 0
fi

wid=$(bspc query -N -n "$id")
if [[ -n "$wid" ]] && bspc query -N -d focused | grep -q "$wid"
then
    bspc node "$id" -g hidden -f
else
    echo z | nc -U /tmp/scrcpy.socket
    bspc node "$id" --to-desktop "$workspace"
    bspc node "$id" -g hidden=off -f
    bspc node "$id" -t floating
    bspc node "$id" -l above
fi

wh=($(xdpyinfo | grep dimensions | sed -r '/^[^0-9]*([0-9]+)x([0-9]+).*/!d;s//\1 \2/;q'))

xy=($(xwininfo -id $id | grep dimensions | sed -r '/^[^0-9]*([0-9]+)x([0-9]+).*/!d;s//\1 \2/;q'))
# w=${wh[0]}
# h=${wh[1]}
# x=$((w/4 + 230))
# y=30
# w=$((w/2))
# h=$((h - 60))
# xdo move -x $x -y $y "$id"
# xdo resize -w $w -h $h "$id"
