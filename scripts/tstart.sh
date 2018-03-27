#!/usr/bin/env bash

tmux new -d -s htop htop;
tmux new -d -s emacs emacsclient -a '' -t -c;
tmux new-session -A -s amos
