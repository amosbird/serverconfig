#!/usr/bin/env bash

workspace=$(bspc query -D -d focused --names)
if pgrep wemeetapp &>/dev/null; then
    while read -r wid; do
        winfo=$(xprop -id "$wid")
        if grep -E -q "window state: (Normal|Iconic)" <<<"$winfo"; then
            if grep -F -q "_NET_WM_NAME(UTF8_STRING) = \"腾讯会议\"" <<< "$winfo"; then
                found=1
                break
            fi
        fi
    done < <(xdo id -N wemeetapp -n wemeetapp)
    if [ -z "$found" ]; then
        /opt/wemeet/wemeetapp.sh &
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
    x=$((w / 12))
    y=100
    w=$((w * 33 / 40))
    h=$((h * 17 / 20))
    xdo move -x $x -y $y "$wid"
    xdo resize -w $w -h $h "$wid"
    bspc node "$wid".window -f
    bspc node "$wid" -l above
else
    rm /tmp/wemeet
    # env FONTCONFIG_FILE=~/.config/tgfonts.conf
    bash -c "/opt/wemeet/wemeetapp.sh &> /dev/null"
fi
