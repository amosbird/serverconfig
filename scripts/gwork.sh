#!/usr/bin/env bash

HOST=100.88.203.53

remote_home=$(ssh $HOST 'pkill -f "kitty -o allow_remote_control=yes --listen-on unix:/tmp/kitty_sock -T local"; rm -f $HOME/tmp/{clipservice.sock,kitty_sock,dbus_sock}; echo $HOME')

UUID=$(uuidgen)
SSH_MASTER_CTRL=/tmp/ssh-master.$UUID
KITTY_SOCK=/tmp/kitty.$UUID
kitty -o allow_remote_control=yes --listen-on unix:$KITTY_SOCK -T local ssh -t $HOST -M -S $SSH_MASTER_CTRL \
	-R $remote_home/tmp/clipservice.sock:/tmp/clipservice.sock \
	-R $remote_home/tmp/kitty_sock:$KITTY_SOCK \
	-R $remote_home/tmp/dbus_sock:/run/user/1000/bus \
	"SSH_MASTER_CTRL=$SSH_MASTER_CTRL SSH_SERVER=$HOST tstart.sh remote_local" &
