#!/usr/bin/env bash

urxvt -T 201.nobida.cn -e ssh -t 201.nobida.cn "tmux new -d -s emacs emacs ; tmux new-session -A -s amos"
