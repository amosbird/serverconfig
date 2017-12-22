#!/usr/bin/env bash

tmux neww -n Debug
tmux splitw -h -p 50
tmux select-pane -t 1
tty1=$(tmux display -p "#{pane_tty}")
tmux splitw -v -p 40
tty3=$(tmux display -p "#{pane_tty}")
tmux select-pane -t 1
tmux splitw -v -p 40
tty2=$(tmux display -p "#{pane_tty}")
# tmux send-keys "reptyr -l" C-m
tmux send-keys "sleep infinity" C-m
tmux select-pane -t 4
tmux send-keys "cgdb -- $1 -tty $tty2 -ex \"dashboard -output $tty1\" -ex \"dashboard expression -output $tty3\"" C-m
# echo $tty1
# echo $tty2
# echo $tty3
