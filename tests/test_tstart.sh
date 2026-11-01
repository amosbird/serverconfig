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

if "$ROOT/scripts/tstart.sh" remote host /tmp/control > /dev/null 2>&1; then
    echo 'legacy remote unexpectedly accepted a missing web-forward port' >&2
    exit 1
fi
