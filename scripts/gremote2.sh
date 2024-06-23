#!/usr/bin/env bash

# NEED OPENSSH >= 6.7 TO FORWARD GPG SOCKET
# set -x
a=
if [ "$1" = "-h" ]; then
	a="--hold"
	shift
fi
pattern='^(([[:alnum:]]+)@)?([^:^@]+)(:([[:digit:]]+))?$'
if [[ "$1" =~ $pattern ]]; then
	user=${BASH_REMATCH[2]}
	host=${BASH_REMATCH[3]}
	port=${BASH_REMATCH[5]}
	if [ -n "$user" ]; then
		arg="$user@$host"
	else
		arg="$host"
	fi

	if [ -n "$port" ]; then
		arg="$arg -p $port"
	fi

	{
		read -r remote_home
		read -r sock
	} < <(
		ssh $arg 'read sock < <($HOME/tmp/gentoo/prelogin); rm $sock; rm $HOME/tmp/{clipservice.sock,ssh_auth_sock,dbus_sock,kitty_sock}; echo $HOME; echo $sock'
	)
	# NOTE: make sure remote should not have gpg-agent, and gpg-connect-agent should show: connection to the agent is in restricted mode

	# kitty $a bash -c "echo $remote_home; echo amosbird; echo $sock"

	# TODO: dbus requires same UID
	# DBUS_SESSION_BUS_ADDRESS

	UUID=$(uuidgen)
	SSH_MASTER_CTRL=/tmp/ssh-master.$UUID
	KITTY_SOCK=/tmp/kitty.$UUID
	kitty $a -o allow_remote_control=yes --listen-on unix:$KITTY_SOCK -T remote ssh -t $arg -M -S $SSH_MASTER_CTRL -L 127.0.0.1:8123:127.0.0.1:8123 -L 127.0.0.1:5601:127.0.0.1:5601 -R 12639:localhost:8888 -R 5353:localhost:53 -R $sock:$(gpgconf --list-dir agent-extra-socket) -R $remote_home/tmp/clipservice.sock:/tmp/clipservice.sock -R $remote_home/tmp/ssh_auth_sock:$SSH_AUTH_SOCK -R $remote_home/tmp/kitty_sock:$KITTY_SOCK -R $remote_home/tmp/dbus_sock:/run/user/1000/bus "$remote_home/tmp/gentoo/login prefix $arg $remote_home/tmp/gentoo/login $SSH_MASTER_CTRL"
fi
