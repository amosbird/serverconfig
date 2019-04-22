#!/usr/bin/env bash

termite -t 221 -e "ssh 172.26.149.127 -t 'env -i HOME=\$HOME TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\" /home/amos/gentoo/startprefix'"
./g122.sh:3:termite -t 122 -e "ssh 10.61.2.122 -t 'env -i HOME=\$HOME TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\"
