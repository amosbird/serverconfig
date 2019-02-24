#!/usr/bin/env bash

if ! tmux list-sessions | grep -q -F emacs
then
    fuser -k /tmp/emacs.lock # sometimes emacs daemon doesn't quit
    tmux new -d -s emacs fish -i -c startemacs
fi
tmux switch-client -t emacs
