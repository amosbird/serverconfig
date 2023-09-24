#!/usr/bin/env bash

if ! tmux list-sessions | grep -q -F emacs; then
    pkill -9 -F "$TMPDIR/emacs-server"
    tmux new -d -s emacs "exec startemacs"
fi
tmux switch-client -t emacs -Z
