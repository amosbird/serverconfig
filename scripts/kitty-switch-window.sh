#!/usr/bin/env bash

if [ -z "$1" ]; then
    echo "Usage: $0 <kitty_window_id> <target_window_id>"
    exit 1
fi

current_window_id=$1
target_window_id=$2

if [ "$current_window_id" -eq 1 ]; then
    :
elif [ "$current_window_id" -eq 2 ]; then
    if [ "$target_window_id" -eq 2 ]; then
        target_window_id=1
    fi
elif [ "$current_window_id" -eq 3 ]; then
    if [ "$target_window_id" -eq 3 ]; then
        target_window_id=1
    fi
elif [ "$current_window_id" -eq 4 ]; then
    if [ "$target_window_id" -eq 4 ]; then
        target_window_id=1
    fi
fi

if ! kitty @ focus-window --match id:"$target_window_id"; then
    echo "Failed to switch to window with ID $target_window_id"
    exit 1
else
    echo "Switched to window with ID $target_window_id"
fi
