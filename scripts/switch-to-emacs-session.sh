#!/usr/bin/env bash

if ! tmux list-sessions | grep -q -F emacs
then
    pkill -f "emacs --daemon"  # sometimes emacs daemon doesn't quit
    tmux new -d -s emacs fish -i -c "emacsclient -a '' -t -c";
fi
tmux switch-client -t emacs
