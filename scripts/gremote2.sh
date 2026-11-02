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
    remote_display=$ssh_target

    {
        read -r remote_home
        read -r sock
        read -r remote_abush_web_socket
        read -r remote_abush_web_socket_status
    } < <(
        ssh "${ssh_options[@]}" "$ssh_target" '
            source "$HOME/.config/environment.sh"
            source "$HOME/.config/path.sh" remote
            read sock < <($HOME/scripts/prelogin)
            rm "$sock"
            rm -f /tmp/{clipservice.sock,ssh_auth_sock,dbus_sock,kitty_sock}
            abush_web_socket=$XDG_RUNTIME_DIR/abush/web-ingress.sock
            if [[ ! -S $abush_web_socket ]]; then
                "$HOME/git/abush/target/release/abush" </dev/null >/dev/null 2>&1 || true
            fi
            if [[ ! -e $abush_web_socket ]]; then
                abush_web_socket_status=missing
            elif [[ ! -S $abush_web_socket ]]; then
                abush_web_socket_status=not-a-socket
            elif [[ $(stat -c %u -- "$abush_web_socket") != $(id -u) ]]; then
                abush_web_socket_status=wrong-owner
            else
                abush_web_socket_status=ready
            fi
            printf "%s\n" "$HOME" "$sock" "$abush_web_socket" "$abush_web_socket_status"
        '
    )
    # NOTE: make sure remote should not have gpg-agent, and gpg-connect-agent should show: connection to the agent is in restricted mode

    # kitty $a bash -c "echo $remote_home; echo amosbird; echo $sock"

    # TODO: dbus requires same UID
    # DBUS_SESSION_BUS_ADDRESS

    readonly ABUSH_WEB_FORWARD_PORT=17890
    abush_web_forward=()
    abush_workspace_arg=(--without-abush)
    preview_error=
    if [[ ! $remote_abush_web_socket =~ ^/[A-Za-z0-9._/-]+$ ]]; then
        preview_error="remote Abush ingress socket has an unsafe path: $remote_abush_web_socket"
    elif [[ $remote_abush_web_socket_status != ready ]]; then
        preview_error="remote Abush ingress socket $remote_abush_web_socket is $remote_abush_web_socket_status; start Abush on the remote host and verify the socket is user-owned"
    elif ! python3 - "$ABUSH_WEB_FORWARD_PORT" <<'PY'
import socket
import sys

sock = socket.socket()
try:
    sock.bind(("127.0.0.1", int(sys.argv[1])))
except OSError:
    sys.exit(1)
finally:
    sock.close()
PY
    then
        preview_error="local port $ABUSH_WEB_FORWARD_PORT is already in use; stop its owner or choose a different Abush forward port"
    else
        abush_web_forward=(-L "127.0.0.1:${ABUSH_WEB_FORWARD_PORT}:${remote_abush_web_socket}")
        abush_workspace_arg=(--abush-web-forward-port "$ABUSH_WEB_FORWARD_PORT")
    fi
    if [[ -n $preview_error ]]; then
        printf 'gremote2: Abush preview unavailable for %s: %s\n' "$remote_display" "$preview_error" >&2
        if [[ ${GREMOTE_REQUIRE_PREVIEW:-0} == 1 ]]; then
            exit 1
        fi
        printf 'gremote2: continuing without the remote Abush TUI (set GREMOTE_REQUIRE_PREVIEW=1 to require it)\n' >&2
    fi

    UUID=$(uuidgen)
    SSH_MASTER_CTRL=/tmp/ssh-master.$UUID
    KITTY_SOCK=/tmp/kitty.$UUID
    kitty_args=()
    if [ -n "$a" ]; then
        kitty_args+=("$a")
    fi
    tstart_ssh_port_arg=()
    if [[ -n $port ]]; then
        tstart_ssh_port_arg=(--ssh-port "$port")
    fi
    printf -v remote_command '%q ' \
        "$remote_home/scripts/tstart.sh" workspace remote \
        --display "$remote_display" \
        --ssh-control "$SSH_MASTER_CTRL" \
        "${tstart_ssh_port_arg[@]}" \
        "${abush_workspace_arg[@]}"

    kitty "${kitty_args[@]}" -o allow_remote_control=yes --listen-on "unix:$KITTY_SOCK" \
        -T remote ssh -t "${ssh_options[@]}" -o ExitOnForwardFailure=yes -M -S "$SSH_MASTER_CTRL" \
        "${abush_web_forward[@]}" \
        -L 127.0.0.1:8123:127.0.0.1:8123 -L 127.0.0.1:57201:127.0.0.1:57201 -L 127.0.0.1:8040:127.0.0.1:8040 \
        -L 127.0.0.1:8030:127.0.0.1:8030 -L 127.0.0.1:8929:127.0.0.1:8929 -L 127.0.0.1:9200:127.0.0.1:9200 \
        -L 127.0.0.1:9400:127.0.0.1:9400 -L 127.0.0.1:9411:127.0.0.1:9411 -L 127.0.0.1:1455:127.0.0.1:1455 \
        -L 127.0.0.1:5601:127.0.0.1:5601 -R 127.0.0.1:8787:127.0.0.1:8787 -R 12639:localhost:8888 \
        -R 5353:localhost:53 -R 9222:localhost:9222 -R $sock:$(gpgconf --list-dir agent-extra-socket) \
        -R /tmp/clipservice.sock:/tmp/clipservice.sock -R /tmp/ssh_auth_sock:$SSH_AUTH_SOCK \
        -R /tmp/kitty_sock:$KITTY_SOCK -R /tmp/dbus_sock:/run/user/1000/bus \
        "$ssh_target" "$remote_command"
fi
