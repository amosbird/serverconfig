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
    ssh_target=$host
    if [ -n "$user" ]; then
        ssh_target="$user@$host"
    fi
    ssh_options=()
    if [ -n "$port" ]; then
        ssh_options=(-p "$port")
    fi
    remote_display="$ssh_target${port:+:$port}"

    {
        read -r remote_home
        read -r sock
    } < <(
        ssh "${ssh_options[@]}" "$ssh_target" 'read sock < <($HOME/scripts/prelogin); rm "$sock"; rm -f /tmp/{clipservice.sock,ssh_auth_sock,dbus_sock,kitty_sock}; echo "$HOME"; echo "$sock"'
    )
    # NOTE: make sure remote should not have gpg-agent, and gpg-connect-agent should show: connection to the agent is in restricted mode

    # kitty $a bash -c "echo $remote_home; echo amosbird; echo $sock"

    # TODO: dbus requires same UID
    # DBUS_SESSION_BUS_ADDRESS

    preview_forward=()
    preview_error=
    remote_http_socket=$(ssh "${ssh_options[@]}" "$ssh_target" \
        '$HOME/scripts/preview gateway ensure --quiet --print-http-socket' 2>&1) || \
        preview_error="remote gateway setup failed: $remote_http_socket"
    if [ -z "$preview_error" ]; then
        if [[ ! "$remote_http_socket" =~ ^/[A-Za-z0-9._/-]+$ ]]; then
            preview_error="remote gateway returned an unsafe socket path"
        elif ! python3 - <<'PY'
import socket
import sys

sock = socket.socket()
try:
    sock.bind(("127.0.0.1", 17890))
except OSError:
    sys.exit(1)
finally:
    sock.close()
PY
        then
            preview_error="local 127.0.0.1:17890 is already in use"
        else
            # This preflight cannot eliminate a bind race before the main SSH starts.
            # GREMOTE_REQUIRE_PREVIEW makes initialization failures fatal; it does not
            # change the failure policy of this session's other forwards.
            preview_forward=(-L "127.0.0.1:17890:$remote_http_socket")
        fi
    fi
    if [ -n "$preview_error" ]; then
        printf 'gremote2: preview unavailable for %s: %s\n' "$remote_display" "$preview_error" >&2
        if [ "${GREMOTE_REQUIRE_PREVIEW:-0}" = 1 ]; then
            exit 1
        fi
    fi

    UUID=$(uuidgen)
    SSH_MASTER_CTRL=/tmp/ssh-master.$UUID
    KITTY_SOCK=/tmp/kitty.$UUID
    kitty_args=()
    if [ -n "$a" ]; then
        kitty_args+=("$a")
    fi
    kitty "${kitty_args[@]}" -o allow_remote_control=yes --listen-on "unix:$KITTY_SOCK" \
        -T remote ssh -t "${ssh_options[@]}" -M -S "$SSH_MASTER_CTRL" \
        "${preview_forward[@]}" \
        -L 127.0.0.1:8123:127.0.0.1:8123 -L 127.0.0.1:57201:127.0.0.1:57201 -L 127.0.0.1:8040:127.0.0.1:8040 \
        -L 127.0.0.1:8030:127.0.0.1:8030 -L 127.0.0.1:8929:127.0.0.1:8929 -L 127.0.0.1:9200:127.0.0.1:9200 \
        -L 127.0.0.1:9400:127.0.0.1:9400 -L 127.0.0.1:9411:127.0.0.1:9411 -L 127.0.0.1:1455:127.0.0.1:1455 \
        -L 127.0.0.1:5601:127.0.0.1:5601 -R 127.0.0.1:8787:127.0.0.1:8787 -R 12639:localhost:8888 \
        -R 5353:localhost:53 -R 9222:localhost:9222 -R $sock:$(gpgconf --list-dir agent-extra-socket) \
        -R /tmp/clipservice.sock:/tmp/clipservice.sock -R /tmp/ssh_auth_sock:$SSH_AUTH_SOCK \
        -R /tmp/kitty_sock:$KITTY_SOCK -R /tmp/dbus_sock:/run/user/1000/bus \
        "$ssh_target" "$remote_home/scripts/tstart.sh remote $remote_display $SSH_MASTER_CTRL"
fi
