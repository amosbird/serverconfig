#!/usr/bin/env bash
# Verify that the installed SmartDNS really forwards every query and preserves upstream TTL.
set -euo pipefail

WORK=$(mktemp -d)
PORT=$((20000 + $$ % 20000))
UPSTREAM=$((PORT + 1))
PIDS=()
cleanup() {
    local pid
    for pid in "${PIDS[@]}"; do kill "$pid" 2>/dev/null || true; done
    for _ in $(seq 1 20); do
        local alive=false
        for pid in "${PIDS[@]}"; do kill -0 "$pid" 2>/dev/null && alive=true; done
        $alive || break
        sleep 0.1
    done
    for pid in "${PIDS[@]}"; do kill -KILL "$pid" 2>/dev/null || true; done
    for pid in "${PIDS[@]}"; do wait "$pid" 2>/dev/null || true; done
    rm -rf "$WORK"
}
trap cleanup EXIT INT TERM

for command in smartdns python3 dig timeout; do
    command -v "$command" >/dev/null || { echo "missing dependency: $command" >&2; exit 1; }
done

cat >"$WORK/upstream.py" <<'PY'
import socket, struct, sys
port, count_path = int(sys.argv[1]), sys.argv[2]
sock = socket.socket(socket.AF_INET, socket.SOCK_DGRAM)
sock.bind(("127.0.0.1", port))
while True:
    query, peer = sock.recvfrom(4096)
    if len(query) < 12:
        continue
    offset = 12
    while offset < len(query) and query[offset]:
        offset += query[offset] + 1
    question_end = offset + 5
    question = query[12:question_end]
    with open(count_path, "a") as count:
        count.write("1\n")
    header = query[:2] + struct.pack("!HHHHH", 0x8180, 1, 1, 0, 0)
    answer = b"\xc0\x0c" + struct.pack("!HHIH", 1, 1, 37, 4) + socket.inet_aton("192.0.2.55")
    sock.sendto(header + question + answer, peer)
PY
python3 "$WORK/upstream.py" "$UPSTREAM" "$WORK/count" &
PIDS+=("$!")
for _ in $(seq 1 20); do
    kill -0 "${PIDS[0]}" 2>/dev/null || { echo 'fake DNS upstream exited' >&2; exit 1; }
    ss -lun "sport = :$UPSTREAM" | grep -q ":$UPSTREAM" && break
    sleep 0.05
done
ss -lun "sport = :$UPSTREAM" | grep -q ":$UPSTREAM" || {
    echo 'fake DNS upstream did not become ready' >&2
    exit 1
}

cat >"$WORK/smartdns.conf" <<EOF
bind 127.0.0.1:$PORT -no-cache -no-serve-expired
bind-tcp 127.0.0.1:$PORT -no-cache -no-serve-expired
cache-size 0
cache-persist no
prefetch-domain no
serve-expired no
rr-ttl-min 0
speed-check-mode none
server 127.0.0.1:$UPSTREAM
log-level error
log-console yes
EOF
smartdns -f -p - -c "$WORK/smartdns.conf" >"$WORK/smartdns.log" 2>&1 &
PIDS+=("$!")

for _ in $(seq 1 30); do
    if dig +time=1 +tries=1 +short @127.0.0.1 -p "$PORT" cache-test.invalid A \
        2>/dev/null | grep -Fxq 192.0.2.55; then
        ready=true
        break
    fi
    sleep 0.1
done
${ready:-false} || { echo 'SmartDNS test instance did not become ready' >&2; exit 1; }
: >"$WORK/count"
for _ in 1 2 3; do
    answer=$(dig +time=2 +tries=1 +noall +answer @127.0.0.1 -p "$PORT" cache-test.invalid A)
    ttl=$(awk '$4 == "A" {print $2; exit}' <<<"$answer")
    [ "$ttl" = 37 ] || { echo "TTL changed: ${ttl:-missing}" >&2; exit 1; }
done
queries=$(wc -l <"$WORK/count")
[ "$queries" -eq 3 ] || { echo "upstream saw $queries queries, expected 3" >&2; exit 1; }
printf 'OK   SmartDNS forwards every query and preserves upstream TTL\n'
