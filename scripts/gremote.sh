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
	port=${BASH_REMATCH[5]:-22}

	ssh $host -p $port '$HOME/gentoo/usr/bin/gpgconf --create-socketdir; sleep 10;' &

    remote_sock=$(ssh $host -p $port '$HOME/gentoo/usr/bin/gpgconf --create-socketdir; file=$($HOME/gentoo/usr/bin/gpgconf --list-dir agent-socket); rm $file; echo $file;')

	# ssh -o ServerAliveInterval=30 -o ServerAliveCountMax=60 -A -t $host -p $port -R $remote_sock:$(gpgconf --list-dir agent-extra-socket)
	termite $a -t $1 -e "ssh -o ServerAliveInterval=30 -o ServerAliveCountMax=60 -A -t $host -p $port -R 10000:localhost:8080 -L 9000:localhost:19000 -L 8123:localhost:18123 -R $remote_sock:$(gpgconf --list-dir agent-extra-socket) -R /tmp/clipservice.sock:/tmp/clipservice.sock 'env -i TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\" SSH_AUTH_SOCK=\"\$SSH_AUTH_SOCK\" SSH_REMOTE_HOST=\"\$(hostname)\" \$HOME/gentoo/startprefix'"
	# termite $a -t $1 -e "ssh -o ServerAliveInterval=30 -o ServerAliveCountMax=60 -A -t $host -R 9874:127.0.0.1:9874 'env -i TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\" SSH_AUTH_SOCK=\"\$SSH_AUTH_SOCK\" SSH_REMOTE_HOST=\"\$(hostname)\" \$HOME/gentoo/startprefix'"
fi
