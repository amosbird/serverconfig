#!/usr/bin/env bash

workspace=$(bspc query -D -d focused --names)
if pgrep -f WeChat &>/dev/null; then
    while read -r wid; do
        x=$(xprop -id "$wid")
        grep -E -q "window state: (Normal|Iconic)" <<<"$x" && grep -q 'WM_NAME(COMPOUND_TEXT) = "微信"' <<<"$x" && ! grep -q "WM_TRANSIENT_FOR(WINDOW)" <<<"$x" && found=1 && break
    done < <(xdo id -N Wine -n wechat.exe)
    if [ -z "$found" ]; then
        $HOME/.deepinwine/deepin-wine-helper/sendkeys.sh w wechat 3 &>/dev/null
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
    rm /tmp/wechat
    # env FONTCONFIG_FILE=~/.config/tgfonts.conf
    bash -c "wechat.sh &> /dev/null"
fi
