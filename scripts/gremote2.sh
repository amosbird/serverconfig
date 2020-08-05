#!/usr/bin/env bash

# NEED OPENSSH >= 6.7 TO FORWARD GPG SOCKET
# set -x
a=
if [ "$1" = "-h" ];
then
    a="--hold"
    shift
fi
pattern='^(([[:alnum:]]+)@)?([^:^@]+)(:([[:digit:]]+))?$'
if [[ "$1" =~ $pattern ]]; then
    user=${BASH_REMATCH[2]}
    host=${BASH_REMATCH[3]}
    port=${BASH_REMATCH[5]:-22}

    ssh $host -p $port '/tmp/gentoo/usr/bin/gpgconf --create-socketdir; sleep 10;' &

    remote_sock=$(ssh $host -p $port '/tmp/gentoo/usr/bin/gpgconf --create-socketdir; file=$(/tmp/gentoo/usr/bin/gpgconf --list-dir agent-socket); rm $file; echo $file;')

    termite $a -t $1 -e "ssh -A -t $host -p $port -R 10000:localhost:8080 -R $remote_sock:$(gpgconf --list-dir agent-extra-socket) '/tmp/gentoo/startprefix'"
fi
