#!/usr/bin/env bash

termite -t phone -e "ssh -t root@phone 'env -i TERM=\$TERM SSH_CONNECTION=\"\$SSH_CONNECTION\" /data/gentoo64/startprefix'"
