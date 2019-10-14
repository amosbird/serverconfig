#!/usr/bin/env bash

# don't forward for myself
termite -t t480 -e "ssh -t t480 'bash -c \"env SHELL=/home/amos/gentoo/usr/local/bin/fish /home/amos/scripts/tstart.sh\"'"
