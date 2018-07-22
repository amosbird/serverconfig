#!/usr/bin/env bash

# urxvt -T 10.61.2.123 -e ssh -t 10.61.2.123 /home/amos/scripts/tstart.sh
alacritty -t 10.61.2.123 -e ssh -t 10.61.2.123 'bash -l /home/amos/scripts/tstart.sh'
# st -T 10.61.2.123 -e ssh -t 10.61.2.123 /home/amos/scripts/tstart.sh

# need to installterm first
# termite -t 10.61.2.123 -e "ssh -t 10.61.2.123 /home/amos/scripts/tstart.sh"
