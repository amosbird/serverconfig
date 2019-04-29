#!/usr/bin/env bash

wid=$1
class=$2
instance=$3
title=$(xprop -id "$wid" WM_NAME | perl -ne 'print /"(.*)"/')
echo $class > /tmp/wowow

fc() {
    wh=($(xdpyinfo | grep dimensions | sed -r '/^[^0-9]*([0-9]+)x([0-9]+).*/!d;s//\1 \2/;q'))
    w=${wh[0]}
    h=${wh[1]}
    x=$((w*3/16))
    y=30
    w=$((w*5/8))
    h=$((h - 60))
    echo "layer=above state=floating sticky=on rectangle=${w}x${h}+$x+$y"
}

case "$class" in
    mpv)
        echo "$wid" > /tmp/mpv
        echo "state=floating sticky=on"
        ;;
    feh)
        echo "$wid" > /tmp/feh
        echo "state=floating sticky=on"
        ;;
    obs)
        echo "$wid" >> /tmp/obs
        fc
        ;;
    Conky)
        echo "$wid" > /tmp/conky
        # setbg.sh reset_conky
        echo "desktop=n"
        ;;
    stalonetray)
        echo "$wid" > /tmp/stalonetray
        # setbg.sh reset_tray
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
    URxvt)
        case "$title" in
            urxvt_scratchpad)
                echo "$wid" > /tmp/urxvt_scratchpad
                echo "state=floating hidden=on"
                ;;
            stardict)
                echo "$wid" > /tmp/stardict
                echo "sticky=on state=floating hidden=on"
                compton-trans -w "$wid" 70
                ;;
        esac
        ;;
    Sakura)
        case "$title" in
            201.nobida.cn*)
                echo "desktop=d follow=on"
                ;;
            urxvt_scratchpad)
                echo "$wid" > /tmp/urxvt_scratchpad
                echo "state=floating hidden=on"
                ;;
            stardict)
                echo "$wid" > /tmp/stardict
                echo "sticky=on state=floating hidden=on"
                compton-trans -w "$wid" 70
                ;;
        esac
        ;;
    Termite)
        case "$title" in
            local)
                echo "desktop=e follow=on"
                ;;
            weechat)
                echo "desktop=c follow=on"
                ;;
            201.nobida.cn*)
                echo "desktop=d follow=on"
                ;;
            vim)
                echo "$wid" > /tmp/vim
                echo "state=floating"
                ;;
            urxvt_scratchpad)
                echo "$wid" > /tmp/urxvt_scratchpad
                echo "state=floating hidden=on"  # hidden for show shell logic
                ;;
            stardict)
                echo "$wid" > /tmp/stardict
                echo "sticky=on state=floating hidden=on"
                compton-trans -w "$wid" 70
                ;;
        esac
        ;;
    scrcpy)
        echo "$wid" > /tmp/scrcpy
        compton-trans -w "$wid" 100
        echo "sticky=on state=floating"
        ;;
    TelegramDesktop)
        echo "$wid" >> /tmp/telegram # appending so that sub windows don't overwrite
        echo "sticky=on state=floating"
        ;;
    Soffice)
        wh=($(xdpyinfo | grep dimensions | sed -r '/^[^0-9]*([0-9]+)x([0-9]+).*/!d;s//\1 \2/;q'))
        w=${wh[0]}
        h=${wh[1]}
        x=$((w*3/16))
        y=30
        w=$((w*5/8))
        h=$((h - 60))
        echo "rectangle=${w}x${h}+$x+$y"
        ;;
    Emacs)
        case "$title" in
            mail-frame)
                echo "desktop=v state=tiled follow=on"
                ;;
            popup)
                echo "$wid" > /tmp/popup
                fc
                ;;
            emacs-editor)
                wh=($(xdpyinfo | grep dimensions | sed -r '/^[^0-9]*([0-9]+)x([0-9]+).*/!d;s//\1 \2/;q'))
                w=${wh[0]}
                h=${wh[1]}
                x=$((w*3/12))
                y=80
                w=$((w*4/8))
                h=$((h - 120))
                echo "layer=above state=floating sticky=on rectangle=${w}x${h}+$x+$y"
                ;;
        esac
        ;;
esac
