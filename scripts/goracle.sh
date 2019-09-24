#!/usr/bin/env bash

# urxvt -T 201.nobida.cn -e ssh -t 201.nobida.cn /home/amos/scripts/tstart.sh
# alacritty -t 201.nobida.cn -e ssh -t 201.nobida.cn /home/amos/scripts/tstart.sh
# st -T 201.nobida.cn -e ssh -t 201.nobida.cn /home/amos/scripts/tstart.sh

# need to installterm first
# termite -t 201.nobida.cn -e "ssh -t 201.nobida.cn /home/amos/scripts/tstart.sh"
termite -t oracle.wentropy.com -e "ssh -t oracle.wentropy.com 'env -i HOME=\$HOME TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\" /home/amos/gentoo/startprefix'"
# gnome-terminal -t 201.nobida.cn -e "ssh -t 201.nobida.cn /home/amos/scripts/tstart.sh"
# sakura -t 201.nobida.cn -e "ssh -t 201.nobida.cn /home/amos/scripts/tstart.sh"
