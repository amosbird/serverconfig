#!/usr/bin/env bash

termite -t 221 -e "ssh 172.26.149.127 -t 'env -i HOME=\$HOME TERM=\$TERM USER=\$USER /home/amos/gentoo/startprefix'"
