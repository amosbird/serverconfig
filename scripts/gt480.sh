#!/usr/bin/env bash

termite -t t480 -e "ssh -t t480 'fish -c \"env SHELL=/usr/local/bin/fish /home/amos/scripts/tstart.sh\"'"
