#!/usr/bin/env bash

if ! tmux list-sessions | grep -q -F emacs
then
    export DISPLAY=":0"
    tmux new -d -s emacs emacsclient -a '' -t -c;
fi
tmux switch-client -t emacs
