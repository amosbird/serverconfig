#!/usr/bin/env bash

set -euo pipefail

ROOT=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
TEST_TMP="$ROOT/.tmp/test-gremote2.$$"
trap 'rm -rf "$TEST_TMP"' EXIT
mkdir -p "$TEST_TMP/bin"

cat > "$TEST_TMP/bin/ssh" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$@" > "$SSH_LOG"
printf '%s\n' /home/remote /tmp/gpg.sock /run/user/1000/abush/web-ingress.sock ready
EOF

cat > "$TEST_TMP/bin/python3" <<'EOF'
#!/usr/bin/env bash
exit 0
EOF

cat > "$TEST_TMP/bin/uuidgen" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' test-uuid
EOF

cat > "$TEST_TMP/bin/gpgconf" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' /tmp/local-gpg.sock
EOF

cat > "$TEST_TMP/bin/kitty" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' "$@" > "$KITTY_LOG"
EOF

chmod +x "$TEST_TMP/bin/ssh" "$TEST_TMP/bin/python3" "$TEST_TMP/bin/uuidgen" \
    "$TEST_TMP/bin/gpgconf" "$TEST_TMP/bin/kitty"
export PATH="$TEST_TMP/bin:$PATH"
export SSH_LOG="$TEST_TMP/ssh.log"
export KITTY_LOG="$TEST_TMP/kitty.log"
export SSH_AUTH_SOCK=/tmp/local-agent.sock

"$ROOT/scripts/gremote2.sh" 'testuser@example.test:2222'

grep -Fxq -- '-p' "$SSH_LOG"
grep -Fxq -- '2222' "$SSH_LOG"
grep -Fxq -- 'testuser@example.test' "$SSH_LOG"
grep -Fq -- '--display testuser@example.test' "$KITTY_LOG"
grep -Fq -- '--ssh-port 2222' "$KITTY_LOG"
if grep -Fq -- 'testuser@example.test:2222' "$KITTY_LOG"; then
    echo 'gremote2 passed user@host:port as an OpenSSH destination' >&2
    exit 1
fi
