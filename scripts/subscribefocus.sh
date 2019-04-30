#!/usr/bin/env bash

bspc subscribe node_focus | while read -r a b c id; do
    compton-trans -w "$id" 100
done
