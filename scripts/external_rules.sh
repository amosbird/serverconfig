#!/usr/bin/env bash

wid=$1
class=$2
instance=$3
title=$(xtitle "$wid")
case "$class" in
    Conky)
        echo $wid > /tmp/conky
        # setbg.sh reset_conky
        echo "desktop=n"
        ;;
    stalonetray)
        echo $wid > /tmp/stalonetray
        setbg.sh reset_tray
        echo "desktop=n layer=above"
        ;;
    qutebrowser)
        case "$title" in
            qbdaemon*)
                echo "hidden = on"
                ;;
            *)
                echo "split_dir=east"
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
        case "$title" in
            weechat)
                echo "desktop=c follow=on"
                ;;
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
