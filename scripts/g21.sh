#!/usr/bin/env bash

termite -t 21 -e "ssh 172.26.190.29 -t 'env -i HOME=\$HOME TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\" /home/amos/gentoo/startprefix'"
