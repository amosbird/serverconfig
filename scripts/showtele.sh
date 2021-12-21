#!/usr/bin/env bash

workspace=$(bspc query -D -d focused --names)
# if pgrep telegram-deskto >/dev/null; then
if pgrep Telegram >/dev/null; then
    while read -r wid
    do
        xprop -id "$wid" | grep -E -q "window state: (Normal|Iconic)" && found=1 && break
    done < <(xdo id -N TelegramDesktop -n Telegram)
    if [ -z "$found" ]; then
        /home/amos/git/tdesktop/out/Release/Telegram
    elif bspc query -N -n focused | grep -q "$wid"; then
        bspc node "$wid" -g hidden -f
        exit 0
    else
        bspc node "$wid" --to-desktop "$workspace"
        bspc node "$wid" -t floating
        bspc node "$wid" -g hidden=off -f
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
    bspc node "$wid" -l above
else
    rm /tmp/telegram
    # env FONTCONFIG_FILE=~/.config/tgfonts.conf
    bash -c "/home/amos/git/tdesktop/out/Release/Telegram &"
    # bash -c "Telegram &"
fi
