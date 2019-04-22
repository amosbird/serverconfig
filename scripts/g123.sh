#!/usr/bin/env bash

termite -t 123 -e "ssh 10.61.2.123 -t 'env -i HOME=\$HOME TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\" /home/amos/gentoo/startprefix'"
