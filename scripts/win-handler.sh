#!/usr/bin/env bash

if pgrep xfreerdp
then
    printf "cmd /c start %s\n" "$1" > /dev/udp/127.0.0.1/9333;
    bspc desktop --focus w
else
    notify-send -a "Remote Windows" "There is no remote Windows"
fi
