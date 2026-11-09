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
    destination=$1
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

    ssh_config=$(ssh -G "${ssh_options[@]}" "$ssh_target") || {
        printf 'gremote2: could not resolve the SSH connection identity\n' >&2
        exit 1
    }
    canonical_user=$(awk '$1 == "user" { print $2; exit }' <<< "$ssh_config")
    canonical_host=$(awk '$1 == "hostname" { print $2; exit }' <<< "$ssh_config")
    canonical_port=$(awk '$1 == "port" { print $2; exit }' <<< "$ssh_config")
    if [[ ! $canonical_user =~ ^[^[:space:][:cntrl:]]+$ ]] ||
        [[ ! $canonical_host =~ ^[^[:space:][:cntrl:]]+$ ]] ||
        [[ ! $canonical_port =~ ^[0-9]+$ ]]; then
        printf 'gremote2: ssh -G returned an invalid connection identity\n' >&2
        exit 1
    fi
    remote_hash=$(
        printf '%s\0%s\0%s' "$canonical_user" "$canonical_host" "$canonical_port" |
            sha256sum
    ) || exit 1
    remote_hash=${remote_hash%% *}
    if [[ ! $remote_hash =~ ^[0-9a-f]{64}$ ]]; then
        printf 'gremote2: could not hash the SSH connection identity\n' >&2
        exit 1
    fi
    remote_key="r_${remote_hash:0:24}"

    {
        read -r remote_home
        read -r sock
        read -r remote_runtime_dir
    } < <(
        ssh "${ssh_options[@]}" "$ssh_target" '
            read sock < <($HOME/scripts/prelogin)
            rm "$sock"
            rm -f /tmp/{clipservice.sock,ssh_auth_sock,dbus_sock,kitty_sock}
            printf "%s\n" "$HOME" "$sock" "$XDG_RUNTIME_DIR"
        '
    )
    # NOTE: make sure remote should not have gpg-agent, and gpg-connect-agent should show: connection to the agent is in restricted mode

    # kitty $a bash -c "echo $remote_home; echo amosbird; echo $sock"

    # TODO: dbus requires same UID
    # DBUS_SESSION_BUS_ADDRESS

    readonly ABUSH_WEB_FORWARD_PORT=17890
    if [[ ! $remote_runtime_dir =~ ^/[A-Za-z0-9._/-]+$ ]] ||
        [[ $remote_runtime_dir == / ]] || [[ $remote_runtime_dir == *//* ]] ||
        [[ $remote_runtime_dir == */./* ]] || [[ $remote_runtime_dir == */../* ]] ||
        [[ $remote_runtime_dir == */. ]] || [[ $remote_runtime_dir == */.. ]]; then
        printf 'gremote2: remote runtime directory has an unsafe path: %s\n' \
            "$remote_runtime_dir" >&2
        exit 1
    fi
    readonly REMOTE_ABUSH_WEB_SOCKET="$remote_runtime_dir/abush/web-ingress.sock"
    readonly REMOTE_BROWSER_SOCKET="$remote_runtime_dir/abush/browser-peer.sock"

    if [[ ! ${XDG_RUNTIME_DIR:-} =~ ^/[A-Za-z0-9._/-]+$ ]] ||
        [[ $XDG_RUNTIME_DIR == / ]] || [[ $XDG_RUNTIME_DIR == *//* ]] ||
        [[ $XDG_RUNTIME_DIR == */./* ]] || [[ $XDG_RUNTIME_DIR == */../* ]] ||
        [[ $XDG_RUNTIME_DIR == */. ]] || [[ $XDG_RUNTIME_DIR == */.. ]]; then
        printf 'gremote2: XDG_RUNTIME_DIR is not a safe absolute path\n' >&2
        exit 1
    fi

    UUID=$(uuidgen) || exit 1
    uuid_hex=${UUID//-/}
    if [[ ! $uuid_hex =~ ^[A-Za-z0-9]{8,64}$ ]]; then
        printf 'gremote2: uuidgen returned an unsafe value\n' >&2
        exit 1
    fi
    connection_id="c_$uuid_hex"
    connection_nonce=$UUID
    browser_remotes_dir="$XDG_RUNTIME_DIR/abush/browser-remotes"
    remote_dir="$browser_remotes_dir/$remote_key"
    connection_dir="$remote_dir/$connection_id"
    browser_socket="$connection_dir/browser-peer.sock"
    SSH_MASTER_CTRL="$connection_dir/ssh-control.sock"
    KITTY_SOCK="$connection_dir/kitty.sock"
    metadata="$connection_dir/connection.json"

    python3 - "$XDG_RUNTIME_DIR" "$connection_dir" <<'PY'
import os
import stat
import sys

runtime, connection = sys.argv[1:]
uid = os.getuid()
current = os.path.sep
for component in runtime.split(os.path.sep)[1:]:
    current = os.path.join(current, component)
    st = os.lstat(current)
    if stat.S_ISLNK(st.st_mode):
        raise SystemExit(f"gremote2: symlink in XDG_RUNTIME_DIR: {current}")
st = os.lstat(runtime)
if not stat.S_ISDIR(st.st_mode):
    raise SystemExit("gremote2: XDG_RUNTIME_DIR is not a real directory")
if st.st_uid != uid or st.st_mode & 0o077:
    raise SystemExit("gremote2: XDG_RUNTIME_DIR has unsafe ownership or mode")
current = runtime
for component in ("abush", "browser-remotes", os.path.basename(os.path.dirname(connection)),
                  os.path.basename(connection)):
    current = os.path.join(current, component)
    try:
        os.mkdir(current, 0o700)
    except FileExistsError:
        pass
    st = os.lstat(current)
    if not stat.S_ISDIR(st.st_mode) or stat.S_ISLNK(st.st_mode):
        raise SystemExit(f"gremote2: unsafe runtime path component: {current}")
    if st.st_uid != uid or stat.S_IMODE(st.st_mode) != 0o700:
        raise SystemExit(f"gremote2: unsafe runtime directory ownership or mode: {current}")
PY
    if [ $? -ne 0 ]; then
        exit 1
    fi

    created_at=$(date -u +%Y-%m-%dT%H:%M:%SZ) || exit 1
    python3 - "$metadata" "$remote_key" "$connection_id" "$destination" \
        "$canonical_user" "$canonical_host" "$canonical_port" "$browser_socket" \
        "$REMOTE_BROWSER_SOCKET" "$SSH_MASTER_CTRL" "$connection_nonce" "$$" \
        "$created_at" <<'PY'
import json
import os
import sys

(path, remote_key, connection_id, display_name, canonical_user, canonical_host,
 canonical_port, browser_socket, remote_browser_socket, ssh_control,
 connection_nonce, owner_pid, created_at) = sys.argv[1:]
data = {
    "schema_version": 1,
    "remote_key": remote_key,
    "connection_id": connection_id,
    "display_name": display_name,
    "canonical_user": canonical_user,
    "canonical_host": canonical_host,
    "canonical_port": int(canonical_port),
    "browser_socket": browser_socket,
    "remote_browser_socket": remote_browser_socket,
    "ssh_control": ssh_control,
    "connection_nonce": connection_nonce,
    "owner_pid": int(owner_pid),
    "created_at": created_at,
}
temporary = path + ".tmp"
fd = os.open(temporary, os.O_WRONLY | os.O_CREAT | os.O_EXCL, 0o600)
with os.fdopen(fd, "w", encoding="utf-8") as output:
    json.dump(data, output, ensure_ascii=False, sort_keys=True)
    output.write("\n")
    output.flush()
    os.fsync(output.fileno())
os.chmod(temporary, 0o600)
os.replace(temporary, path)
PY
    if [ $? -ne 0 ]; then
        rm -rf -- "$connection_dir"
        exit 1
    fi

    cleanup_connection() {
        python3 - "$connection_dir" "$connection_nonce" <<'PY'
import json
import os
import shutil
import stat
import sys

connection, expected_nonce = sys.argv[1:]
try:
    st = os.lstat(connection)
    if not stat.S_ISDIR(st.st_mode) or stat.S_ISLNK(st.st_mode):
        raise SystemExit
    with open(os.path.join(connection, "connection.json"), encoding="utf-8") as source:
        metadata = json.load(source)
    if metadata.get("connection_nonce") != expected_nonce:
        raise SystemExit
    shutil.rmtree(connection)
except (FileNotFoundError, NotADirectoryError, json.JSONDecodeError, OSError):
    pass
PY
    }
    trap cleanup_connection EXIT

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
        --abush-web-forward-port "$ABUSH_WEB_FORWARD_PORT"

    kitty "${kitty_args[@]}" -o allow_remote_control=yes --listen-on "unix:$KITTY_SOCK" \
        -T remote ssh -t "${ssh_options[@]}" -o ExitOnForwardFailure=yes \
        -o StreamLocalBindUnlink=no -M -S "$SSH_MASTER_CTRL" \
        -L "$browser_socket:$REMOTE_BROWSER_SOCKET" \
        -L "127.0.0.1:${ABUSH_WEB_FORWARD_PORT}:${REMOTE_ABUSH_WEB_SOCKET}" \
        -L 127.0.0.1:8123:127.0.0.1:8123 -L 127.0.0.1:57201:127.0.0.1:57201 -L 127.0.0.1:8040:127.0.0.1:8040 \
        -L 127.0.0.1:8030:127.0.0.1:8030 -L 127.0.0.1:8929:127.0.0.1:8929 -L 127.0.0.1:9200:127.0.0.1:9200 \
        -L 127.0.0.1:9400:127.0.0.1:9400 -L 127.0.0.1:9411:127.0.0.1:9411 -L 127.0.0.1:1455:127.0.0.1:1455 \
        -L 127.0.0.1:5601:127.0.0.1:5601 -R 127.0.0.1:8787:127.0.0.1:8787 -R 12639:localhost:8888 \
        -R 5353:localhost:53 -R 9222:localhost:9222 -R $sock:$(gpgconf --list-dir agent-extra-socket) \
        -R /tmp/clipservice.sock:/tmp/clipservice.sock -R /tmp/ssh_auth_sock:$SSH_AUTH_SOCK \
        -R /tmp/kitty_sock:$KITTY_SOCK -R /tmp/dbus_sock:/run/user/1000/bus \
        "$ssh_target" "$remote_command"
fi
