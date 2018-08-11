#!/usr/bin/env bash

wid=$1
class=$2
instance=$3
title=$(xtitle "$wid")
case "$class" in
    stalonetray)
        echo $wid > /tmp/stalonetray
        wh=($(xdpyinfo | grep dimensions | sed -r 's/^[^0-9]*([0-9]+x[0-9]+).*$/\1/' | awk -Fx '{print $1" "$2}'))
        w=${wh[0]}
        h=${wh[1]}
        x=$((w/2 - 80))
        y=300
        w=0
        h=0
        xdo move -x $x -y $y $wid
        xdo resize -w $w -h $h $wid
        echo "desktop=u"
        ;;
    qutebrowser)
        case "$title" in
            qbdaemon*)
                echo "hidden = on"
                ;;
        esac
        ;;
    Sakura)
        case "$title" in
            201.nobida.cn*)
                echo "desktop=d follow=on"
                ;;
            urxvt_scratchpad)
                echo $wid > /tmp/urxvt_scratchpad
                echo "state=floating hidden=on"
                ;;
            stardict)
                echo $wid > /tmp/stardict
                echo "sticky=on state=floating hidden=on"
                compton-trans -w "$wid" 70
                ;;
        esac
        ;;
    Termite)
        case "$instance" in
            weechat)
                echo "desktop=c follow=on"
                ;;
        esac
        ;;
    TelegramDesktop)
        echo $wid > /tmp/telegram
        echo "sticky=on state=floating hidden=on"
        ;;
    Emacs)
        case "$title" in
            editor-frame)
                echo "desktop=e follow=on"
                ;;
            mail-frame)
                echo "desktop=v follow=on"
                ;;
        esac
        ;;
esac
