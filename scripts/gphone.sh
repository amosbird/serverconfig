#!/usr/bin/env bash

termite -t phone -e "ssh -t root@phone 'env -i TERM=\$TERM SSH_CONNECTION=\"\$SSH_CONNECTION\" SSH_AUTH_SOCK=\"\$SSH_AUTH_SOCK\" /data/gentoo64/startprefix'"
