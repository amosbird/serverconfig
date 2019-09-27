#!/usr/bin/env bash

termite -t t450s -e "ssh -t t450s 'fish -c \"env SHELL=/home/amos/gentoo/usr/local/bin/fish /home/amos/scripts/tstart.sh\"'"
