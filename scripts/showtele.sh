#!/usr/bin/env bash

process_name=telegram-deskto
binary=telegram-desktop
window_name=telegram-desktop
class_name=TelegramDesktop

workspace=$(bspc query -D -d focused --names)
if pgrep $process_name >/dev/null; then
    while read -r wid; do
        xprop -id "$wid" | grep -E -q "window state: (Normal|Iconic)" && found=1 && break
    done < <(xdo id -N $class_name -n $window_name)

    if [ -z "$found" ]; then
        $binary
    elif bspc query -N -n focused | grep -q "$wid"; then
        bspc node older.!hidden -f
        bspc node "$wid".window -g hidden
        exit 0
    else
        bspc node "$wid" -t floating
        bspc node "$wid".window -g hidden=off
        bspc node "$wid" --to-desktop "$workspace"
    fi
    wh=($(xrandr --current | perl -ne 'if (/primary/) {@x=split; $x[3] =~ /(\d+)x(\d+)/; print $1." ".$2}'))
    w=${wh[0]}
    h=${wh[1]}
    x=$((w / 8))
    y=80
    w=$((w * 3 / 4))
    h=$((h - 140))
    xdo move -x $x -y $y "$wid"
    xdo resize -w $w -h $h "$wid"
    bspc node "$wid".window -f
    bspc node "$wid" -l above
else
    rm /tmp/telegram
    # env FONTCONFIG_FILE=~/.config/tgfonts.conf
    # export QT_SCREEN_SCALE_FACTORS=
    # export QT_AUTO_SCREEN_SCALE_FACTOR=
    # bash -c "/home/amos/git/tdesktop/out/Release/Telegram &"
    bash -c "$binary &"
fi
