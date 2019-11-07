#!/usr/bin/env bash

ssh $1 '/home/amos/gentoo/usr/bin/gpgconf --create-socketdir; sleep 10;' &

remote_sock=$(ssh $1 '/home/amos/gentoo/usr/bin/gpgconf --create-socketdir; file=$(/home/amos/gentoo/usr/bin/gpgconf --list-dir agent-socket); rm $file; echo $file;')

termite -t $1 -e "ssh -A -t $1 -R $remote_sock:$(gpgconf --list-dir agent-extra-socket) 'env -i HOME=\$HOME TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\" SSH_AUTH_SOCK=\"\$SSH_AUTH_SOCK\" SSH_REMOTE_HOST=\"\$(hostname)\" /home/amos/gentoo/startprefix'"
