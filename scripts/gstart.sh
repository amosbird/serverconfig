#!/usr/bin/env bash

if [[ "$(hostname)" == "abx1gen3" ]]; then
    killwork
else
    pkill -f "kitty -o allow_remote_control=yes --listen-on unix:/tmp/kitty_sock -T local" || :
fi
rm -f /tmp/kitty_sock

kitty -o allow_remote_control=yes --listen-on unix:/tmp/kitty_sock -T local "$HOME/scripts/tstart.sh" local &

disown

qtile cmd-obj -o group e -f toscreen
