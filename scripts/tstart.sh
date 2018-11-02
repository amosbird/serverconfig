#!/usr/bin/env bash

tmux new -d -s htop htop;
tmux new -d -s emacs fish -i -c "emacsclient -a '' -t -c";
tmux new-session -A -s amos
