#!/usr/bin/env bash

remote_sock=$(ssh $1 'file=$(/home/amos/gentoo/usr/bin/gpgconf --list-dir agent-socket); rm $file; echo $file')
termite -t $1 -e "ssh -t $1 -R $remote_sock:$(gpgconf --list-dir agent-extra-socket) 'env -i HOME=\$HOME TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\" SSH_AUTH_SOCK=\"\$SSH_AUTH_SOCK\" /home/amos/gentoo/startprefix'"
