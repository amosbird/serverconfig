#!/usr/bin/env bash

cat /home/amos/git/serverconfig/colors | fzf --prompt="Color Input > " --reverse --bind "enter:execute-silent( colorinsert 1 {} )+unix-line-discard","alt-enter:execute-silent( colorinsert 2 {} )+unix-line-discard","ctrl-c:execute-silent( colorinsert 3 )+unix-line-discard","ctrl-g:execute-silent( colorinsert 3 )+unix-line-discard","ctrl-q:execute-silent( colorinsert 3 )+unix-line-discard","ctrl-]:execute-silent( colorinsert 3 )+unix-line-discard"
