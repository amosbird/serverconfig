#!/usr/bin/env bash

if ! tmux list-sessions | grep -q -F emacs; then
    # fuser -k -9 /tmp/emacs.lock # sometimes emacs daemon doesn't quit
    kill -9 "$(cat /tmp/emacs-server)"
    tmux new -d -s emacs fish -i -c startemacs
fi
tmux switch-client -t emacs
