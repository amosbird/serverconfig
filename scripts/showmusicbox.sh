#!/usr/bin/env bash

if pgrep -f 'NEMbox'
then
    :
else
    cd ~/git/musicbox/
    alacritty -t musicbox -e python -m NEMbox &
    sleep 0.25
fi

i3-msg '[instance="^musicbox"] scratchpad show'

{ read -r width; read -r height; } < <(i3-msg -t get_workspaces | jq -r 'map(select(.focused))[0].rect["width","height"]')
x=$((width/4))
y=30
width=$((width/2))
height=$((height - 60))

i3-msg "[con_id=\"__focused__\" instance=\"^musicbox\"] move position $x $y"
i3-msg "[con_id=\"__focused__\" instance=\"^musicbox\"] resize set $width $height"
