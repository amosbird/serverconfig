#!/usr/bin/env bash

termite -t 119 -e "ssh 10.61.1.119 -t 'env -i HOME=\$HOME TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\" /home/amos/gentoo/startprefix'"
