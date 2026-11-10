#!/usr/bin/env bash

set -euo pipefail

repo=$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT
root="$tmp/external-links"
bin="$tmp/bin"
mkdir -p "$root" "$bin"
cp -a "$repo/chromium/external-links/extension" "$root/extension"

cat > "$bin/pass" <<'EOF'
#!/usr/bin/env bash
printf '%s\n' fake-private-key
EOF
cat > "$bin/openssl" <<'EOF'
#!/usr/bin/env python3
import base64
import json
import os
import pathlib

manifest = pathlib.Path(os.environ["TEST_MANIFEST"])
os.write(1, base64.b64decode(json.loads(manifest.read_text())["key"]))
EOF
cat > "$bin/jq" <<'EOF'
#!/usr/bin/env python3
import json
import pathlib
import sys

args = sys.argv[1:]
raw = "-r" in args
null_input = "-n" in args
variables = {}
while "--arg" in args:
    index = args.index("--arg")
    variables[args[index + 1]] = args[index + 2]
    del args[index:index + 3]
args = [arg for arg in args if arg not in ("-r", "-n")]
expression = args.pop(0)
data = None if null_input else json.loads(pathlib.Path(args[0]).read_text())
if expression == ".source_hash":
    result = data["source_hash"]
elif expression == ".version":
    result = data["version"]
elif expression == ".version = $version":
    data["version"] = variables["version"]
    result = data
elif expression == "{source_hash: $source_hash, version: $version}":
    result = {"source_hash": variables["source_hash"], "version": variables["version"]}
else:
    raise SystemExit(f"unsupported fake jq expression: {expression}")
if raw and isinstance(result, str):
    print(result)
else:
    json.dump(result, sys.stdout)
    print()
EOF
cat > "$bin/chrome" <<'EOF'
#!/usr/bin/env bash
set -euo pipefail
printf '%s\n' "$*" >> "$CHROME_CALLS"
extension=
profile=
for argument in "$@"; do
    case $argument in
        --headless=new) headless=1 ;;
        --no-first-run) first_run=1 ;;
        --no-default-browser-check) browser_check=1 ;;
        --user-data-dir=*) profile=${argument#*=} ;;
        --pack-extension=*) extension=${argument#*=} ;;
        --pack-extension-key=*) key=${argument#*=} ;;
        --no-sandbox) exit 90 ;;
    esac
done
[[ ${headless:-} && ${first_run:-} && ${browser_check:-} && $profile && $extension && ${key:-} ]]
[[ $profile == "$(dirname "$extension")"/chrome-profile ]]
mkdir -p "$profile"
case ${CHROME_MODE:-success} in
    success) printf 'Cr24\003\000\000\000payload' > "${extension}.crx" ;;
    invalid) printf 'not-a-crx' > "${extension}.crx" ;;
    fail) echo 'fake zygote failure at private temporary paths' >&2; exit 1 ;;
esac
EOF
chmod +x "$bin/pass" "$bin/openssl" "$bin/jq" "$bin/chrome"

export PATH="$bin:$PATH"
export EXTERNAL_LINKS_ROOT="$root"
export CHROME_BIN="$bin/chrome"
export CHROME_CALLS="$tmp/chrome-calls"
export TEST_MANIFEST="$root/extension/manifest.json"

"$repo/scripts/package-external-links"
[[ $(wc -l < "$CHROME_CALLS") -eq 1 ]]
python3 - "$root/package/extension.crx" <<'PY'
import pathlib
import struct
import sys

header = pathlib.Path(sys.argv[1]).read_bytes()[:8]
assert header[:4] == b"Cr24"
assert struct.unpack("<I", header[4:])[0] == 3
PY
python3 - "$root/package/build-state.json" "$root/package/external-extension.json" <<'PY'
import json
import pathlib
import sys

state = json.loads(pathlib.Path(sys.argv[1]).read_text())
external = json.loads(pathlib.Path(sys.argv[2]).read_text())
assert len(state["source_hash"]) == 64
assert external["external_version"] == state["version"]
assert external["external_crx"] == str(pathlib.Path(sys.argv[1]).with_name("extension.crx"))
PY

artifact_hash=$(sha256sum "$root/package/extension.crx")
state_hash=$(sha256sum "$root/package/build-state.json")
"$repo/scripts/package-external-links"
[[ $(wc -l < "$CHROME_CALLS") -eq 1 ]]
[[ $(sha256sum "$root/package/extension.crx") == "$artifact_hash" ]]
[[ $(sha256sum "$root/package/build-state.json") == "$state_hash" ]]

printf '\n' >> "$root/extension/status.js"
for mode in fail invalid; do
    export CHROME_MODE=$mode
    if "$repo/scripts/package-external-links" > "$tmp/$mode.out" 2> "$tmp/$mode.err"; then
        echo "expected $mode Chrome packaging to fail" >&2
        exit 1
    fi
    [[ $(sha256sum "$root/package/extension.crx") == "$artifact_hash" ]]
    [[ $(sha256sum "$root/package/build-state.json") == "$state_hash" ]]
done
grep -q 'Chrome failed to pack External Links extension.' "$tmp/fail.err"
grep -q 'invalid extension.crx' "$tmp/invalid.err"
[[ -z $(find "$root/package" -maxdepth 1 -name '.*.??????' -print -quit) ]]
