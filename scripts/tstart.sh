#!/usr/bin/env bash

tmux new -d -s htop htop;
tmux new -d -s emacs bash -l -c "emacsclient -a '' -t -c";
tmux new-session -A -s amos
