#!/usr/bin/env bash

urxvt -T 201.nobida.cn -e ssh -t 201.nobida.cn "tmux new -d -s htop htop; tmux new -d -s emacs bash -l -c 'emacs -nw'; tmux new-session -A -s amos"
