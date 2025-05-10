#!/usr/bin/env bash

if [ -n "$TMUX" ]; then
    buffer=$(tmux capture-pane -pS -10000)
else
    buffer=$(kitty @ get-text --extent screen | head -n -1)
fi

prev_line=$(echo "$buffer" | tac | grep -m1 -v '^[[:space:]]*$')

echo "$prev_line"
