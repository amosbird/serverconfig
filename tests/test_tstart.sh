#!/usr/bin/env bash

set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
TEST_TMP="$ROOT/.tmp/test-tstart.$$"
trap 'rm -rf "$TEST_TMP"' EXIT
mkdir -p "$TEST_TMP/bin"

cat > "$TEST_TMP/bin/kitten" <<'EOF'
#!/usr/bin/env bash
printf '%q ' "$@" >> "$TEST_LOG"
printf '\n' >> "$TEST_LOG"
EOF

cat > "$TEST_TMP/bin/startabush" <<'EOF'
#!/usr/bin/env bash
printf '%q ' "$@" > "$TEST_LOG"
printf '\n' >> "$TEST_LOG"
EOF

cat > "$TEST_TMP/bin/tmux" <<'EOF'
#!/usr/bin/env bash
exit 0
EOF

chmod +x "$TEST_TMP/bin/kitten" "$TEST_TMP/bin/startabush" "$TEST_TMP/bin/tmux"
export GUI=t
export PATH="$TEST_TMP/bin:$PATH"
export TEST_LOG="$TEST_TMP/invocations.log"

assert_fails() {
    local expected=$1
    shift
    local error="$TEST_TMP/error.log"
    if "$ROOT/scripts/tstart.sh" "$@" > /dev/null 2> "$error"; then
        printf 'tstart unexpectedly accepted: %q ' "$@" >&2
        printf '\n' >&2
        exit 1
    fi
    grep -Fq -- "$expected" "$error"
}

"$ROOT/scripts/tstart.sh" workspace remote \
    --display 'user@host:2222' \
    --ssh-control '/tmp/control path' \
    --abush-web-forward-port 34567

[[ $(wc -l < "$TEST_LOG") -eq 4 ]]
grep -Fq -- 'ssh -S /tmp/control\ path -tt user@host:2222' "$TEST_LOG"
grep -Fq -- '--remote-web-port\ 34567' "$TEST_LOG"

: > "$TEST_LOG"
"$ROOT/scripts/tstart.sh" app abush --remote-web-port 45678
grep -Fxq -- '--remote-web-port 45678 ' "$TEST_LOG"

: > "$TEST_LOG"
"$ROOT/scripts/tstart.sh" local
grep -Fq -- 'app abush --local' "$TEST_LOG"

: > "$TEST_LOG"
"$ROOT/scripts/tstart.sh" workspace remote \
    --display host \
    --ssh-control /tmp/control \
    --without-abush
[[ $(wc -l < "$TEST_LOG") -eq 3 ]]
if grep -Fq -- 'app abush' "$TEST_LOG"; then
    echo 'workspace --without-abush unexpectedly launched Abush' >&2
    exit 1
fi

assert_fails 'invalid port: 0' app abush --remote-web-port 0
assert_fails 'invalid port: 65536' workspace remote --display host --ssh-control ctl --abush-web-forward-port 65536
assert_fails 'invalid port: abc' workspace remote --display host --ssh-control ctl --abush-web-forward-port abc
assert_fails 'requires a value' workspace remote --display host --ssh-control ctl --abush-web-forward-port
assert_fails 'choose exactly one' workspace remote --display host --ssh-control ctl --without-abush --abush-web-forward-port 1234
assert_fails 'specified more than once' workspace remote --display host --display other --ssh-control ctl --without-abush
assert_fails 'requires one of' workspace remote --display host --ssh-control ctl
assert_fails 'legacy remote requires' remote host /tmp/control
