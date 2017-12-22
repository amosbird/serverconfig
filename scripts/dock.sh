#!/bin/bash
# wait for the dock state to change
sleep 1
export DISPLAY=:0
export XAUTHORITY=/home/amos/.Xauthority
DOCKED=$(cat /sys/devices/platform/dock.0/docked)
case "$DOCKED" in
  "1")
    /usr/bin/xrandr --output eDP1 --off --output DP2-2 --primary --mode 1920x1080 --pos 0x0
  ;;
  "0")
    /usr/bin/xrandr --output eDP1 --primary --mode 1600x900 --pos 0x0 --output DP2-2 --off
  ;;
esac
exit 0
