#!/usr/bin/env bash

termite -t 226 -e "ssh 172.26.5.226 -t 'env -i HOME=\$HOME TERM=\$TERM USER=\$USER /home/amos/gentoo/startprefix'"
