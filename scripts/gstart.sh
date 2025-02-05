#!/usr/bin/env bash

pkill -f "kitty -o allow_remote_control=yes --listen-on unix:/tmp/kitty_sock -T local"

kitty -o allow_remote_control=yes --listen-on unix:/tmp/kitty_sock -T local "$HOME/scripts/tstart.sh" local
