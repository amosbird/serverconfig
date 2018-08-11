#!/usr/bin/env bash

if pgrep -f urxvt_scratchpad;
then
    :
else
    sakura -t urxvt_scratchpad -e tmux new -A -s amos &
    sleep 0.5 # i3 issue
fi

id=$(cat /tmp/urxvt_scratchpad);
workspace=$(bspc query -D -d focused --names)
if [[ "$1" -eq 0 ]]
then
    bspc desktop --focus h
    bspc node $id --to-desktop h
    bspc node $id -g hidden=off -f -t tiled
else
    if [ "$workspace" = h ]
    then
        exit 0
    fi
    # if bspc query -N -n focused | grep $(bspc query -N -n $id)
    # then
    #     bspc node $id -g hidden=on
    # fi

    wh=($(xdpyinfo | grep dimensions | sed -r 's/^[^0-9]*([0-9]+x[0-9]+).*$/\1/' | awk -Fx '{print $1" "$2}'))
    w=${wh[0]}
    h=${wh[1]}
    w=$((w/2 - 7))
    h=$((h - 20))
    y=7

    if bspc query -N -d focused | grep -q $(bspc query -N -n $id)
    then
        bspc node $id -g hidden
    else
        bspc node $id -t floating
        bspc node $id -g hidden=off
        bspc node $id --to-desktop $workspace
    fi

    if [[ "$1" -eq  1 ]]
    then
        x=$((w + 7))
    elif [[ "$1" -eq 2 ]]
    then
        x=2
    fi
    xdo move -x $x -y $y $id
    xdo resize -w $w -h $h $id
    bspc node $id -f
fi
