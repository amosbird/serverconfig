#!/usr/bin/env bash

export LANG=en_US.UTF-8
export SHELL
tmux -u new -d -s htop htop;
if ! tmux list-sessions | grep -q -F emacs
then
    fuser -k /tmp/emacs.lock # sometimes emacs daemon doesn't quit
fi
tmux -u new -d -s emacs fish -i -c startemacs
tmux -u new-session -A -s amos
