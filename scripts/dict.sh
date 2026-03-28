#!/usr/bin/env bash

cat ~/.stardict/wordlist | fzf -e --preview "{ echo {} | sdcv -e | sed -n '2,\$p' | head -n -1; wn {} -over | sed -n '2,\$p'; } | fold -s -w 100" --preview-window=up:80% --bind "ctrl-o:kill-line","enter:execute(synonyms {} | sed 's/\x0c/\n/g' | fold -s -w 100 | less)"
