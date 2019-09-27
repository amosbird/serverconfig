#!/usr/bin/env bash

termite -t box -e "ssh -t box 'fish -c \"env SHELL=/home/amos/gentoo/usr/local/bin/fish /home/amos/scripts/tstart.sh\"'"
