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

	remote_home=$(ssh $arg 'rm $HOME/tmp/gentoo/home/amos/.gnupg/S.gpg-agent; rm $HOME/tmp/{clipservice.sock,ssh_auth_sock}; echo $HOME')

	kitty $a -T $1 ssh -t $arg -R 12639:localhost:12639 -R $remote_home/tmp/gentoo/home/amos/.gnupg/S.gpg-agent:$(gpgconf --list-dir agent-extra-socket) -R $remote_home/tmp/clipservice.sock:/tmp/clipservice.sock -R $remote_home/tmp/ssh_auth_sock:$SSH_AUTH_SOCK '$HOME/tmp/gentoo/gentoo_mount $HOME/tmp /tmp/gentoo/startprefix'
fi
