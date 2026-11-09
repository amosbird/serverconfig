#!/usr/bin/env bash

set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
TEST_TMP="$ROOT/.tmp/test-gremote2.$$"
trap 'rm -rf "$TEST_TMP"' EXIT
mkdir -p "$TEST_TMP/bin" "$TEST_TMP/runtime"
chmod 0700 "$TEST_TMP/runtime"

cat > "$TEST_TMP/bin/ssh" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$@" >> "$SSH_LOG"
if [ "${1:-}" = -G ]; then
    target=${!#}
    port=22
    user=effective
    host=${target#*@}
    while [ "$#" -gt 0 ]; do
        if [ "$1" = -p ]; then
            port=$2
            shift 2
        else
            shift
        fi
    done
    if [[ $target == *@* ]]; then
        user=${target%@*}
    fi
    printf 'user %s\nhostname canonical-%s\nport %s\n' "$user" "$host" "$port"
else
    printf '%s\n' /home/remote /tmp/gpg.sock "${FAKE_REMOTE_RUNTIME:-/run/user/1000}"
fi
EOF

cat > "$TEST_TMP/bin/uuidgen" <<'EOF'
#!/usr/bin/env bash
count=0
if [ -f "$UUID_COUNT" ]; then
    read -r count < "$UUID_COUNT"
fi
count=$((count + 1))
printf '%s\n' "$count" > "$UUID_COUNT"
printf '00000000-0000-4000-8000-%012d\n' "$count"
EOF

cat > "$TEST_TMP/bin/gpgconf" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' /tmp/local-gpg.sock
EOF

cat > "$TEST_TMP/bin/kitty" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$@" > "$KITTY_LOG"
browser_socket=
previous=
for argument in "$@"; do
    if [ "$previous" = -L ] && [[ $argument == */browser-peer.sock:* ]]; then
        browser_socket=${argument%%:*}
        break
    fi
    previous=$argument
done
[ -n "$browser_socket" ]
connection_dir=${browser_socket%/browser-peer.sock}
cp "$connection_dir/connection.json" "$CAPTURE_DIR/connection-$KITTY_RUN.json"
printf '%s\n' "$connection_dir" > "$CAPTURE_DIR/directory-$KITTY_RUN"
printf '%s %s\n' \
    "$(stat -c %a "$connection_dir")" \
    "$(stat -c %a "$connection_dir/connection.json")" \
    > "$CAPTURE_DIR/modes-$KITTY_RUN"
if [ -n "${SIBLING_TO_CHECK:-}" ]; then
    [ -d "$SIBLING_TO_CHECK" ]
fi
EOF

chmod +x "$TEST_TMP/bin/ssh" "$TEST_TMP/bin/uuidgen" \
    "$TEST_TMP/bin/gpgconf" "$TEST_TMP/bin/kitty"
export PATH="$TEST_TMP/bin:$PATH"
export SSH_LOG="$TEST_TMP/ssh.log"
export KITTY_LOG="$TEST_TMP/kitty.log"
export UUID_COUNT="$TEST_TMP/uuid-count"
export CAPTURE_DIR="$TEST_TMP/captures"
export XDG_RUNTIME_DIR="$TEST_TMP/runtime"
export SSH_AUTH_SOCK=/tmp/local-agent.sock
mkdir -p "$CAPTURE_DIR"

run_remote() {
    KITTY_RUN=$1 "$ROOT/scripts/gremote2.sh" "$2"
}

run_remote one 'testuser@example.test:2222'
one_dir=$(< "$CAPTURE_DIR/directory-one")
one_metadata="$CAPTURE_DIR/connection-one.json"
[ ! -e "$one_dir" ]
[ "$(< "$CAPTURE_DIR/modes-one")" = '700 600' ]

python3 - "$one_metadata" "$XDG_RUNTIME_DIR" <<'PY'
import json
import pathlib
import sys

metadata_path, runtime = sys.argv[1:]
with open(metadata_path, encoding="utf-8") as source:
    data = json.load(source)
assert data["schema_version"] == 1
assert data["remote_key"].startswith("r_")
assert data["connection_id"].startswith("c_")
assert data["display_name"] == "testuser@example.test:2222"
assert data["canonical_user"] == "testuser"
assert data["canonical_host"] == "canonical-example.test"
assert data["canonical_port"] == 2222
assert data["remote_browser_socket"] == "/run/user/1000/abush/browser-peer.sock"
assert pathlib.Path(data["browser_socket"]).is_relative_to(runtime)
assert pathlib.Path(data["ssh_control"]).is_relative_to(runtime)
assert pathlib.Path(data["browser_socket"]).parent.name == data["connection_id"]
assert pathlib.Path(data["browser_socket"]).parent.parent.name == data["remote_key"]
PY

browser_socket=$(python3 -c \
    'import json,sys; print(json.load(open(sys.argv[1]))["browser_socket"])' \
    "$one_metadata")
grep -Fxq -- '-o' "$KITTY_LOG"
grep -Fxq -- 'StreamLocalBindUnlink=no' "$KITTY_LOG"
grep -Fxq -- "$browser_socket:/run/user/1000/abush/browser-peer.sock" "$KITTY_LOG"
grep -Fxq -- '-p' "$SSH_LOG"
grep -Fxq -- '2222' "$SSH_LOG"
grep -Fxq -- 'testuser@example.test' "$SSH_LOG"
grep -Fq -- '--display testuser@example.test' "$KITTY_LOG"
grep -Fq -- '--ssh-port 2222' "$KITTY_LOG"
if grep -Fq -- 'testuser@example.test:2222' "$KITTY_LOG"; then
    echo 'gremote2 passed user@host:port as an OpenSSH destination' >&2
    exit 1
fi

remote_key_one=$(python3 -c \
    'import json,sys; print(json.load(open(sys.argv[1]))["remote_key"])' \
    "$one_metadata")
connection_one=$(python3 -c \
    'import json,sys; print(json.load(open(sys.argv[1]))["connection_id"])' \
    "$one_metadata")
remote_parent="$XDG_RUNTIME_DIR/abush/browser-remotes/$remote_key_one"
mkdir -m 0700 "$remote_parent/sibling"
SIBLING_TO_CHECK="$remote_parent/sibling" run_remote two 'testuser@example.test:2222'
[ -d "$remote_parent/sibling" ]
connection_two=$(python3 -c \
    'import json,sys; print(json.load(open(sys.argv[1]))["connection_id"])' \
    "$CAPTURE_DIR/connection-two.json")
remote_key_two=$(python3 -c \
    'import json,sys; print(json.load(open(sys.argv[1]))["remote_key"])' \
    "$CAPTURE_DIR/connection-two.json")
[ "$remote_key_one" = "$remote_key_two" ]
[ "$connection_one" != "$connection_two" ]

run_remote three 'other@example.test:2222'
remote_key_three=$(python3 -c \
    'import json,sys; print(json.load(open(sys.argv[1]))["remote_key"])' \
    "$CAPTURE_DIR/connection-three.json")
[ "$remote_key_one" != "$remote_key_three" ]

unsafe_runtime="$TEST_TMP/unsafe-runtime"
mkdir "$unsafe_runtime"
chmod 0755 "$unsafe_runtime"
if XDG_RUNTIME_DIR="$unsafe_runtime" KITTY_RUN=unsafe \
    "$ROOT/scripts/gremote2.sh" example.test >/dev/null 2>&1; then
    echo 'gremote2 accepted an insecure XDG_RUNTIME_DIR' >&2
    exit 1
fi

ln -s "$TEST_TMP/runtime" "$TEST_TMP/runtime-link"
if XDG_RUNTIME_DIR="$TEST_TMP/runtime-link" KITTY_RUN=symlink \
    "$ROOT/scripts/gremote2.sh" example.test >/dev/null 2>&1; then
    echo 'gremote2 accepted a symlink XDG_RUNTIME_DIR' >&2
    exit 1
fi

if XDG_RUNTIME_DIR=relative/runtime KITTY_RUN=relative \
    "$ROOT/scripts/gremote2.sh" example.test >/dev/null 2>&1; then
    echo 'gremote2 accepted a relative XDG_RUNTIME_DIR' >&2
    exit 1
fi

if FAKE_REMOTE_RUNTIME='../unsafe' KITTY_RUN=remote-unsafe \
    "$ROOT/scripts/gremote2.sh" example.test >/dev/null 2>&1; then
    echo 'gremote2 accepted an unsafe remote runtime directory' >&2
    exit 1
fi
