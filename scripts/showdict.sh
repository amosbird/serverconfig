#!/usr/bin/env bash

opt=$1
shift
create=
if ! pgrep -f 'sdcv' >/dev/null; then
    termite -t stardict -e dict.sh &
    sleep 0.5
    create=1
fi

workspace=$(bspc query -D -d focused --names)

id=$(cat /tmp/stardict)
if [[ -z $id ]]; then
    exit 0
fi

case "$opt" in
1)
    less <<<$(synonyms "$1" | sed 's//\n/g' | fold -s -w 100)
    exit 0
    ;;
2)
    bspc node "$id" -g hidden -f
    less <<<$(synonyms "$1" | sed 's//\n/g' | fold -s -w 100)
    exit 0
    ;;
3)
    bspc node "$id" -g hidden -f
    exit 0
    ;;
esac

if [[ -z $create ]] && bspc query -N -n focused | grep -q "$(bspc query -N -n "$id")"; then
    bspc node "$id" -g hidden -f
    exit 0
else
    bspc node "$id" --to-desktop "$workspace"
    bspc node "$id" -t floating
    bspc node "$id" -g hidden=off -f
fi

wh=($(xrandr --current | perl -ne 'if (/primary/) {@x=split; $x[3] =~ /(\d+)x(\d+)/; print $1." ".$2}'))
w=${wh[0]}
h=${wh[1]}
x=$((w / 4))
y=30
w=$((w / 2))
h=$((h - 60))
xdo move -x "$x" -y "$y" "$id"
xdo resize -w "$w" -h "$h" "$id"
bspc node "$id" -l above
