#!/usr/bin/env bash

# NEED OPENSSH >= 6.7 TO FORWARD GPG SOCKET
set -x
a=
if [ "$1" = "-h" ]; then
	a="--hold"
	shift
fi
a="--hold"
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

	if ! ssh $arg 'ls /tmp/gentoo' &> /dev/null
	then
		echo /tmp/gentoo gets removed. Need to set it up manually.
		exit 1
	fi

	ssh $arg '/tmp/gentoo/usr/bin/gpgconf --create-socketdir; sleep 10;' &

	remote_sock=$(ssh $arg '/tmp/gentoo/usr/bin/gpgconf --create-socketdir; file=$(/tmp/gentoo/usr/bin/gpgconf --list-dir agent-socket); rm $file; echo $file; rm /tmp/clipservice.sock;')

	termite $a -t $1 -e "ssh -A -t $arg -R 10000:localhost:8080 -R $remote_sock:$(gpgconf --list-dir agent-extra-socket) -R /tmp/clipservice.sock:/tmp/clipservice.sock 'while :; do touch -h /tmp/gentoo; sleep 60s; done & /tmp/gentoo/startprefix'"
fi
