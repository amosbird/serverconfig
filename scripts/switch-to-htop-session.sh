#!/usr/bin/env bash

if ! tmux list-sessions | grep -q -F htop; then
    tmux -u new -d -s htop "exec starthtop 1"
fi

tmux switch-client -t htop -Z
