#!/usr/bin/env bash

if (($#)); then
    bspc quit 1
else
    bspc wm -d >"$BSPWM_STATE" && bspc quit
fi
