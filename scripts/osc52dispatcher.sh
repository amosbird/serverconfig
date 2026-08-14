#!/usr/bin/env bash

case "$1" in
x)
    luakit "$2"
    ;;
y)
    chromium "$2"
    ;;
z)
    i3-msg focus "$2"
    ;;
*)
    $2
    ;;
esac
