#!/usr/bin/env bash

termite -t 139.9.113.62 -e "ssh -t 139.9.113.62 'env -i HOME=\$HOME TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\" /home/amos/gentoo/startprefix'"
