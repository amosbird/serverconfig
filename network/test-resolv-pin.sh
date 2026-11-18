#!/usr/bin/env bash
# Prove network/resolv-pin leaves /etc/resolv.conf exactly `nameserver 127.0.0.1` and immutable, so
# that SmartGateAgent's attempt to replace it (rename aside, write its own) fails the way it does on
# the live host: `rename /etc/resolv.conf /etc/resolv_ngn_backup.conf: operation not permitted`.
#
# Runs in a sandbox: a private mount namespace with its own tmpfs, under a hard time limit, so the
# host's /etc is never touched. chattr needs a filesystem with inode flags, which tmpfs has.
#
# Usage: test-resolv-pin.sh [resolv-pin]   (the argument exists for mutation testing)
set -euo pipefail

ROOT=$(cd "$(dirname "$0")/.." && pwd)
SCRIPT=$(realpath "${1:-$ROOT/network/resolv-pin}")
LIMIT=${TEST_LIMIT:-30}

fail() { echo "FAIL $*" >&2; exit 1; }
ok() { echo "OK   $*"; }

sandbox() {
    sudo -n timeout -k 5 "$LIMIT" unshare --mount --propagation private \
        env RESOLV_PIN_SANDBOX=1 bash "$0" "$1"
}

if [ "${RESOLV_PIN_SANDBOX:-}" != 1 ]; then
    grep -Fq 'network/resolv-pin' "$ROOT/restore.sh" || fail 'restore.sh does not run resolv-pin'
    ok 'restore.sh runs resolv-pin'
    sandbox "$SCRIPT"
    [ -z "${1:-}" ] || exit 0
    mutate() {
        local work rc=0
        work=$(mktemp)
        sed "$2" "$SCRIPT" >"$work"
        if cmp -s "$SCRIPT" "$work"; then rm -f "$work"; fail "mutant $1 did not change the script"; fi
        sandbox "$work" >/dev/null 2>&1 || rc=$?
        rm -f "$work"
        case $rc in
            0) fail "mutant $1 survived" ;;
            124 | 137) fail "mutant $1 hit the ${LIMIT}s limit instead of failing an assertion" ;;
        esac
    }
    mutate no-immutable 's/"\$CHATTR" +i "\$TARGET"/:/'
    mutate wrong-content 's/nameserver 127.0.0.1/nameserver 192.168.255.10/'
    # No mutant drops the symlink check in correct(): lsattr refuses a symlink, so pinned() already
    # fails for one and that mutant would be equivalent. The symlink cases above cover the behaviour.
    mutate check-ignores-flag 's/pinned || { echo "resolv-pin: \$TARGET is not immutable"; ok=1; }/:/'
    ok "mutation checks: every mutant fails an assertion"
    exit 0
fi

# ---- inside the sandbox ------------------------------------------------------------------------
D=$(mktemp -d)
mount -t tmpfs tmpfs "$D"
T=$D/resolv.conf
pin() { RESOLV_PIN_TARGET=$T bash "$SCRIPT" "$@"; }
# What SmartGateAgent does: move the file aside, then write its own resolvers in its place.
agent_replaces() {
    mv "$T" "$D/resolv_ngn_backup.conf" 2>/dev/null &&
        printf 'nameserver 192.168.255.10\nnameserver 114.114.114.114\n' >"$T"
}
is_immutable() { lsattr -d "$T" | awk '{print $1}' | grep -q i; }

# 1. A stub symlink (systemd-resolved's shape) becomes a pinned regular file.
printf 'nameserver 127.0.0.53\n' >"$D/stub"
ln -s "$D/stub" "$T"
pin >/dev/null
[ ! -L "$T" ] && [ "$(cat "$T")" = 'nameserver 127.0.0.1' ] && is_immutable ||
    fail "a stub symlink was not replaced by a pinned file: $(ls -l "$T"; cat "$T")"
ok 'a resolver symlink is replaced by a pinned regular file'

# 2. SmartGateAgent's replacement fails, and the content survives.
agent_replaces && fail 'SmartGateAgent could move /etc/resolv.conf aside'
[ "$(cat "$T")" = 'nameserver 127.0.0.1' ] || fail "content changed: $(cat "$T")"
[ ! -e "$D/resolv_ngn_backup.conf" ] || fail 'the rename half-succeeded'
ok "SmartGateAgent's rename-and-replace fails and the content survives"
( printf 'x' >>"$T" ) 2>/dev/null && fail 'the pinned file could be appended to'
ok 'the pinned file cannot be written in place either'

# 3. Wrong content under the flag is corrected, and a second run is a no-op.
chattr -i "$T"
printf 'nameserver 8.8.8.8\n' >"$T"
chattr +i "$T"
pin >/dev/null
[ "$(cat "$T")" = 'nameserver 127.0.0.1' ] && is_immutable || fail "wrong content was not corrected: $(cat "$T")"
before=$(stat -c %i "$T")
out=$(pin)
[ -z "$out" ] && [ "$(stat -c %i "$T")" = "$before" ] || fail "a correct pin was rewritten: $out"
ok 'wrong pinned content is corrected; a correct pin is left alone'

# 4. A symlink to a pinned file with the right content is still replaced. `cat` and `lsattr` both
# follow the link and see a correct pin, yet SmartGateAgent can rename the link itself freely, so
# only the type check can tell.
chattr -i "$T"
rm -f "$T"
printf 'nameserver 127.0.0.1\n' >"$D/look-alike"
chattr +i "$D/look-alike"
ln -s "$D/look-alike" "$T"
pin >/dev/null
[ ! -L "$T" ] && is_immutable || fail "a symlink with the right content was left in place: $(ls -l "$T")"
chattr -i "$D/look-alike"
ok 'a symlink is replaced even when it points at a pinned file with the right content'

# 5. --check reports, and fails, on each way the pin can be lost.
pin --check >/dev/null || fail '--check rejected a correct pin'
chattr -i "$T"
pin --check >/dev/null && fail '--check accepted a file that is not immutable'
chattr +i "$T"
ok '--check passes a correct pin and fails a missing flag'
chattr -i "$T"
umount "$D"
