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
        read -r remote_abush_web_socket
    } < <(
        ssh "${ssh_options[@]}" "$ssh_target" 'read sock < <($HOME/scripts/prelogin); rm "$sock"; rm -f /tmp/{clipservice.sock,ssh_auth_sock,dbus_sock,kitty_sock}; printf "%s\n" "$HOME" "$sock" "$XDG_RUNTIME_DIR/abush/web-ingress.sock"'
    )
    # NOTE: make sure remote should not have gpg-agent, and gpg-connect-agent should show: connection to the agent is in restricted mode

    # kitty $a bash -c "echo $remote_home; echo amosbird; echo $sock"

    # TODO: dbus requires same UID
    # DBUS_SESSION_BUS_ADDRESS

    if [[ ! $remote_abush_web_socket =~ ^/[A-Za-z0-9._/-]+$ ]]; then
        printf 'gremote2: remote Abush web socket has an unsafe path: %s\n' "$remote_abush_web_socket" >&2
        exit 1
    fi

    readonly ABUSH_WEB_FORWARD_PORT=17890
    abush_web_forward=(-L "127.0.0.1:${ABUSH_WEB_FORWARD_PORT}:${remote_abush_web_socket}")

    UUID=$(uuidgen)
    SSH_MASTER_CTRL=/tmp/ssh-master.$UUID
    KITTY_SOCK=/tmp/kitty.$UUID
    kitty_args=()
    if [ -n "$a" ]; then
        kitty_args+=("$a")
    fi
    printf -v remote_command '%q ' \
        "$remote_home/scripts/tstart.sh" workspace remote \
        --display "$remote_display" \
        --ssh-control "$SSH_MASTER_CTRL" \
        --abush-web-forward-port "$ABUSH_WEB_FORWARD_PORT"

    kitty "${kitty_args[@]}" -o allow_remote_control=yes --listen-on "unix:$KITTY_SOCK" \
        -T remote ssh -t "${ssh_options[@]}" -M -S "$SSH_MASTER_CTRL" \
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
