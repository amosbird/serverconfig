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
	if [ -n "$user" ]
	then
		arg="$user@$host"
	else
		arg="$host"
	fi

	if [ -n "$port" ]
	then
		arg="$arg -p $port"
	fi

	{
		read -r remote_home;
		read -r sock
	} < <(
		ssh $arg 'read sock < <($HOME/tmp/gentoo/prelogin); rm $sock; rm $HOME/tmp/{clipservice.sock,ssh_auth_sock}; echo $HOME; echo $sock'
	)

	# kitty $a bash -c "echo $remote_home; echo amosbird; echo $sock"

	kitty $a -T $1 ssh -t $arg -L 127.0.0.1:8123:$host:8123 -R 12639:localhost:12639 -R $sock:$(gpgconf --list-dir agent-extra-socket) -R $remote_home/tmp/clipservice.sock:/tmp/clipservice.sock -R $remote_home/tmp/ssh_auth_sock:$SSH_AUTH_SOCK '$HOME/tmp/gentoo/login'
fi
