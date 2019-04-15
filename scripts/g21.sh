#!/usr/bin/env bash

termite -t 21 -e "ssh 172.26.190.29 -t 'env -i HOME=\$HOME TERM=\$TERM USER=\$USER /home/amos/gentoo/startprefix'"
