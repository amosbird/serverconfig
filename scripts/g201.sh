#!/usr/bin/env bash

# urxvt -T 201.nobida.cn -e ssh -t 201.nobida.cn /home/amos/scripts/tstart.sh
# alacritty -t 201.nobida.cn -e ssh -t 201.nobida.cn /home/amos/scripts/tstart.sh
# st -T 201.nobida.cn -e ssh -t 201.nobida.cn /home/amos/scripts/tstart.sh

# need to installterm first
# termite -t 201.nobida.cn -e "ssh -t 201.nobida.cn /home/amos/scripts/tstart.sh"
# gnome-terminal -t 201.nobida.cn -e "ssh -t 201.nobida.cn /home/amos/scripts/tstart.sh"
sakura -t 201.nobida.cn -e "ssh -t 201.nobida.cn /home/amos/scripts/tstart.sh"
