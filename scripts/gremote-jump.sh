#!/usr/bin/env bash

# NEED OPENSSH >= 6.7 TO FORWARD GPG SOCKET
set -x
pattern='^(([[:alnum:]]+)@)?([^:^@]+)(:([[:digit:]]+))?$'
if [[ "$1" =~ $pattern ]]; then
    user=${BASH_REMATCH[2]}
    host=${BASH_REMATCH[3]}
    port=${BASH_REMATCH[5]}

    if [ -z "$user" ]
    then
        user=$USER
    fi
    if [ -z "$port" ]
    then
        port=22
    fi

    if [ "$user" = "$USER" ]
    then
        ssh $user@$host -p $port '/home/amos/gentoo/usr/bin/gpgconf --create-socketdir; sleep 10;' &

        remote_sock=$(ssh $user@$host -p $port '/home/amos/gentoo/usr/bin/gpgconf --create-socketdir; file=$(/home/amos/gentoo/usr/bin/gpgconf --list-dir agent-socket); rm $file; echo $file;')

        # always use local HOME so that prefix might be shared for other users
        termite -t $1 -e "ssh -A -J wentropy.com -t $user@$host -p $port -R $remote_sock:$(gpgconf --list-dir agent-extra-socket) 'env -i TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\" SSH_AUTH_SOCK=\"\$SSH_AUTH_SOCK\" SSH_REMOTE_HOST=\"\$(hostname)\" /home/amos/gentoo/startprefix'"
    else
        # notify-send -a "$0" "Currently not supported"
        # exit 1
        # termite -t $1 -e "ssh -t $user@$host -p $port 'env -i TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\" SSH_AUTH_SOCK=\"\$SSH_AUTH_SOCK\" SSH_REMOTE_HOST=\"\$(hostname)\" /home/amos/gentoo/startprefix'"
        termite -t $1 -e "ssh -t -J wentropy.com $user@$host -p $port 'env -i TERM=\$TERM USER=\$USER SSH_CONNECTION=\"\$SSH_CONNECTION\" SSH_AUTH_SOCK=\"\$SSH_AUTH_SOCK\" SSH_REMOTE_HOST=\"\$(hostname)\" /home/amos/gentoo/startprefix'"
    fi
fi
