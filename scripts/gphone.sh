#!/usr/bin/env bash

# don't forward for myself
termite -t phone -e "adb shell -t su -c source /data/gentoo64/startprefix"
