#!/usr/bin/env bash

termite -t 206 -e "ssh 10.61.2.206 -t 'env -i HOME=\$HOME TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\" /home/amos/gentoo/startprefix'"
