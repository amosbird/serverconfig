#!/usr/bin/env bash

wid=$1
class=$2
instance=$3
title=$(xprop -id "$wid" WM_NAME | perl -ne 'print /"(.*)"/')
net_title=$(xprop -id "$wid" _NET_WM_NAME | perl -ne 'print /"(.*)"/')

fc() {
    wh=($(xrandr --current | perl -ne 'if (/primary/) {@x=split; $x[3] =~ /(\d+)x(\d+)/; print $1." ".$2}'))
    w=${wh[0]}
    h=${wh[1]}
    x=$((w * 3 / 16))
    y=30
    w=$((w * 5 / 8))
    h=$((h - 60))
    echo "layer=above state=floating sticky=on rectangle=${w}x${h}+$x+$y"
}

case "$class" in
discord)
    echo "$wid" >/tmp/discord
    echo "state=floating sticky=on"
    ;;
mpv)
    echo "$wid" >/tmp/mpv
    # echo "state=floating sticky=on"
    echo "state=tiled"
    ;;
feh)
    echo "$wid" >/tmp/feh
    echo "state=floating sticky=on"
    ;;
Blueberry.py)
    echo "$wid" >/tmp/blueberry
    echo "state=floating sticky=on"
    ;;
Blueman-manager)
    echo "$wid" >/tmp/blueman
    echo "state=floating sticky=on"
    ;;
obs)
    echo "$wid" >>/tmp/obs
    fc
    ;;
iOALinux)
    echo "$wid" >> /tmp/ioa
    echo "state=floating sticky=on"
    ;;
kitty)
    case "$title" in
    local)
        echo "desktop=e follow=on"
        ;;
    weechat)
        echo "desktop=c follow=on"
        ;;
    vim)
        echo "$wid" >/tmp/vim
        echo "state=floating"
        ;;
    colorinsert)
        echo "$wid" >/tmp/colorinsert
        echo "state=floating"
        ;;
    urxvt_scratchpad)
        echo "$wid" >/tmp/urxvt_scratchpad
        echo "state=floating hidden=on" # hidden for show shell logic
        ;;
    stardict)
        echo "$wid" >/tmp/stardict
        echo "sticky=on state=floating hidden=on"
        compton-trans -w "$wid" 70
        ;;
    esac
    ;;
scrcpy)
    echo "$wid" >/tmp/scrcpy
    compton-trans -w "$wid" 100
    echo "sticky=on state=floating"
    ;;
TelegramDesktop)
    echo "$wid" >>/tmp/telegram # appending so that sub windows don't overwrite
    echo "sticky=on state=floating"
    ;;
stalonetray)
    echo "$wid" >>/tmp/stalonetray # appending so that sub windows don't overwrite
    echo "sticky=on state=floating"
    ;;
wemeetapp)
    case "$net_title" in
    EmojiFloatWnd)
        echo "hidden = on"
        ;;
    wemeetapp)
        echo "$wid" >>/tmp/wemeet # appending so that sub windows don't overwrite
        echo "sticky=on state=floating"
        ;;
    *)
        echo "$wid" >>/tmp/wemeet # appending so that sub windows don't overwrite
        echo "sticky=on state=floating"
        ;;
    esac
    ;;
esac
