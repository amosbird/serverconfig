#!/usr/bin/env bash

workspace=$(bspc query -D -d focused --names)

id=$(cat /tmp/scrcpy)
if [[ -z $id ]]; then
    echo z | nc -U /tmp/scrcpy.socket
    exit 0
fi

wid=$(bspc query -N -n "$id")
if [[ -n $wid ]] && bspc query -N -n focused | grep -q "$wid"; then
    bspc node "$id" -g hidden -f
else
    echo z | nc -U /tmp/scrcpy.socket
    bspc node "$id" --to-desktop "$workspace"
    bspc node "$id" -t floating
    bspc node "$id" -g hidden=off -f
fi

wh=($(xrandr --current | perl -ne 'if (/primary/) {@x=split; $x[3] =~ /(\d+)x(\d+)/; print $1." ".$2}'))
bspc node "$id" -l above
# w=${wh[0]}
# h=${wh[1]}
# x=$((w/4 + 230))
# y=30
# w=$((w/2))
# h=$((h - 60))
# xdo move -x $x -y $y "$id"
# xdo resize -w $w -h $h "$id"
