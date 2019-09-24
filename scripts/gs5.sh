#!/usr/bin/env bash

termite -t s5 -e "ssh -t s5 'env -i HOME=\$HOME TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\" /home/amos/gentoo/startprefix'"
