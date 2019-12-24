#!/usr/bin/env bash

cat ~/.stardict/wordlist | fzf -e --preview "{ echo {} | sdcv -e | sed -n '2,\$p' | head -n -1; wn {} -over | sed -n '2,\$p'; } | fold -s -w 100" --preview-window=up:80% --bind "ctrl-o:kill-line","enter:execute(showdict.sh 1 {})","ctrl-c:execute-silent(showdict.sh 3 {q})+unix-line-discard","ctrl-g:execute-silent(showdict.sh 3 {q})+unix-line-discard","ctrl-q:execute-silent(showdict.sh 3 {q})+unix-line-discard","ctrl-]:execute-silent(showdict.sh 3 {q})+unix-line-discard"
