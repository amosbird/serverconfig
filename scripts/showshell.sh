#!/usr/bin/env bash

if pgrep -f urxvt_scratchpad;
then
    :
else
    sakura -t urxvt_scratchpad -e fish -c "tmux new -A -s amos" &
    sleep 0.25 # i3 issue
fi

{ read -r width; read -r height; } < <(i3-msg -t get_workspaces | jq -r 'map(select(.focused))[0].rect["width","height"]')
width=$((width/2 - 7))
height=$((height-20))

workspace=$(i3-msg -t get_workspaces | jq -r 'map(select(.focused))[0].name')

if [[ "$1" -eq 0 ]]
then
    # dock
    i3-msg '[title="^urxvt_scratchpad"]' move container to workspace "", floating disable, workspace "", border none
else
    if [ "$workspace"  = "" ]
    then
        exit 0
    fi

    if i3-msg -t get_tree | jq -r --arg v "$workspace" '.nodes[].nodes[].nodes[] | .type == "workspace" and .name == $v and .floating_nodes[].nodes[].window_properties.title=="urxvt_scratchpad"' | grep -q true
    then
        i3-msg '[title="^urxvt_scratchpad"]' scratchpad show
    else
        # undock, pop up
        i3-msg '[title="^urxvt_scratchpad"]' move to scratchpad, focus, border pixel 5
        i3-msg focus mode_toggle
        i3-msg focus mode_toggle
        if [[ "$1"  -eq  1 ]]
        then
            i3-msg '[con_id="__focused__" title="^urxvt_scratchpad"]' move position $((width + 7)) 42, resize set $width $height
        elif [[ "$1" -eq 2 ]]
        then
            i3-msg '[con_id="__focused__" title="^urxvt_scratchpad"]' move position 2 42, resize set $width $height
        fi
    fi
fi
