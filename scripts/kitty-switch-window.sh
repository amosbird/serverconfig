#!/usr/bin/env bash

# set -x

# source /home/amos/scripts/bashlog


# Check if window ID is provided
if [ -z "$1" ]; then
    echo "Usage: $0 <kitty_window_id>"
    exit 1
fi

current_window_id=$1

if [ "$current_window_id" -eq 1 ]; then
    target_window_id=$2
else
    target_window_id=1
fi

# Switch to the target window
kitty @ focus-window --match id:$target_window_id

# Check if the command succeeded
if [ $? -ne 0 ]; then
    echo "Failed to switch to window with ID $target_window_id"
    exit 1
else
    echo "Switched to window with ID $target_window_id"
fi
