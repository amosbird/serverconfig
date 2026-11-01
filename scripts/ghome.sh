#!/usr/bin/env bash

HOST=100.91.94.87

ssh $HOST '$HOME/scripts/killwork; rm -f /tmp/{kitty_sock,dbus_sock,remote-clipservice.sock}'

UUID=$(uuidgen)
SSH_MASTER_CTRL=/tmp/ssh-master.$UUID
KITTY_SOCK=/tmp/kitty.$UUID
kitty -o allow_remote_control=yes --listen-on unix:$KITTY_SOCK -T work ssh -t $HOST -M -S $SSH_MASTER_CTRL \
	-R /tmp/remote-clipservice.sock:/tmp/clipservice.sock \
	-R /tmp/kitty_sock:$KITTY_SOCK \
	-R /tmp/dbus_sock:/run/user/1000/bus \
	"source /tmp/gentoo/etc/profile; PATH=/home/amos/scripts:\$PATH /home/amos/scripts/tstart.sh workspace remote --display $HOST --ssh-control $SSH_MASTER_CTRL --without-abush" &
disown
qtile cmd-obj -o group v -f toscreen
