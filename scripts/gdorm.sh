#!/usr/bin/env bash

# don't forward for myself
termite -t dorm -e "ssh -t dorm 'env -i HOME=\$HOME TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\" /home/amos/gentoo/startprefix'"
