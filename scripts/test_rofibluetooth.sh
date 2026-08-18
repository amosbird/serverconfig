#!/usr/bin/env bash

set -euo pipefail

repo=$(cd "$(dirname "$0")/.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

cat >"$tmp/bluetoothctl" <<'EOF'
#!/usr/bin/env bash
[[ $* == show ]] && printf 'Powered: yes\n'
EOF

cat >"$tmp/rofibluetooth-blocks" <<'EOF'
#!/usr/bin/env bash
printf '{"lines":[{"text":"↻  Scan","data":"scan"}]}\n'
cat >/dev/null
EOF

cat >"$tmp/rofi" <<'EOF'
#!/usr/bin/env bash
IFS= read -r initial
printf '%s\n' "$initial" >"$TEST_MENU"
EOF

cat >"$tmp/notify-send" <<'EOF'
#!/usr/bin/env bash
:
EOF

chmod +x "$tmp"/*
export PATH="$tmp:$PATH" TEST_MENU="$tmp/menu"
"$repo/scripts/rofibluetooth"

grep -q '↻  Scan' "$TEST_MENU"
grep -q 'rofi -modi blocks -show blocks' "$repo/scripts/rofibluetooth"
grep -q 'lazy.spawn("rofibluetooth")' "$repo/.config/qtile/config.py"
grep -q 'pipewire-audio pipewire-alsa pipewire-pulse wireplumber' "$repo/restore.sh"
grep -q 'systemctl enable --now bluetooth.service' "$repo/restore.sh"
