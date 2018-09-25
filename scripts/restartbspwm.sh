#!/usr/bin/env bash

if (( $# == 0 ))
then
    bspc wm -d > "$BSPWM_STATE" && bspc quit
else
    bspc quit 1
fi
