#!/usr/bin/env bash

if pgrep -f urxvt_scratchpad; then
    :
else
    # --debug-font-fallback
    kitty -T urxvt_scratchpad bash -c "env SHELL=/tmp/gentoo/bin/fish tmux -f /home/amos/.tmux/.tmux.conf.gui -L gui new -A -s gui" &
    sleep 0.5 # sleep for show shell logic
fi

id=$(cat /tmp/urxvt_scratchpad)
if [[ -z $id ]]; then
    exit 0
fi

workspace=$(bspc query -D -d focused --names)
if [[ $1 -eq 0 ]]; then
    bspc desktop --focus h
    bspc node "$id".window --to-desktop h
    bspc node "$id".window -g hidden=off -f -t tiled -l normal
else
    if [ "$workspace" = h ]; then
        exit 0
    fi
    wh=($(xrandr --current | perl -ne 'if (/primary/) {@x=split; $x[3] =~ /(\d+)x(\d+)/; print $1." ".$2}'))
    w=${wh[0]}
    h=${wh[1]}
    w=$((w / 2 - 7))
    h=$((h - 20))
    y=10
    read -r lr uv < <(xwininfo -id "$id" | perl -ne 'print $1>2?1:2 if /Absolute upper-left X: (.*)/; print " 1" if /IsViewable/; print " 0" if /IsUnMapped/;')
    if bspc query -N -n focused | grep -q "$(bspc query -N -n "$id")" && [[ $lr -eq $1 ]]; then
        bspc node older.!hidden -f
        bspc node "$id".window -g hidden
        exit 0
    else
        bspc node "$id".window -t floating
        bspc node "$id".window -g hidden=off
        bspc node "$id".window --to-desktop "$workspace"
    fi
    if [[ $1 -eq 1 ]]; then
        x=$((w + 7))
    elif [[ $1 -eq 2 ]]; then
        x=2
    fi
    xdo move -x "$x" -y "$y" "$id"
    xdo resize -w "$w" -h "$h" "$id"
    bspc node "$id".window -f
    bspc node "$id".window -l above
fi
