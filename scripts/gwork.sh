#!/usr/bin/env bash

if [[ "$(hostname)" == "abx1gen3" ]]; then
    HOST=100.91.94.87
else
    HOST=100.88.203.53
fi

ssh $HOST '$HOME/scripts/killwork; rm -f /tmp/{kitty_sock,dbus_sock,remote-clipservice.sock}'

UUID=$(uuidgen)
SSH_MASTER_CTRL=/tmp/ssh-master.$UUID
KITTY_SOCK=/tmp/kitty.$UUID
kitty -o allow_remote_control=yes --listen-on unix:$KITTY_SOCK -T work ssh -t $HOST -M -S $SSH_MASTER_CTRL \
	-R /tmp/remote-clipservice.sock:/tmp/clipservice.sock \
	-R /tmp/kitty_sock:$KITTY_SOCK \
	-R /tmp/dbus_sock:/run/user/1000/bus \
	"/home/amos/scripts/tstart.sh remote $HOST $SSH_MASTER_CTRL" &
disown
qtile cmd-obj -o group v -f toscreen
