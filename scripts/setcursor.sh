#!/usr/bin/env bash

printf '\ePtmux;\e\e]12;#b8860b\007\e\\' >"$1"
printf '\ePtmux;\e\e[2 q\e\\' >"$1"
