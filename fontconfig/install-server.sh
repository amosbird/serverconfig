#!/usr/bin/env bash
# Server-only. Do not run on the Arch desktop.
#
# Idempotent: safe to re-run. Installs the font set that .config/fontconfig
# already expects, plus the SC-prefer drop-in. Nothing here is deployed by
# restore.sh.

set -euo pipefail

if [[ -f /etc/os-release ]] && grep -qx 'ID=arch' /etc/os-release; then
    echo "refusing: this is the Arch desktop. Fonts there come from paru, not this script." >&2
    exit 1
fi

for t in sudo dnf curl tar xz python3 fc-cache fc-match; do
    command -v "$t" >/dev/null || { echo "missing required tool: $t" >&2; exit 1; }
done

here=$(cd "$(dirname "$0")" && pwd)
fonts_dir="${XDG_DATA_HOME:-$HOME/.local/share}/fonts"
conf_dst=/etc/fonts/conf.d/70-noto-cjk-sc-prefer.conf
nf_url=https://github.com/ryanoasis/nerd-fonts/releases/latest/download/NerdFontsSymbolsOnly.tar.xz
ubuntu_url=https://assets.ubuntu.com/v1/fad7939b-ubuntu-font-family-0.83.zip

# Roboto and Roboto Slab are the sans/serif that .config/fontconfig names first.
# Without them Symbols Nerd Font, which has no letters, becomes the generic
# default and "fc-match sans-serif" returns a font that cannot render text.
sudo dnf install -y \
    google-noto-cjk-fonts \
    google-noto-emoji-color-fonts \
    google-noto-sans-mono-cjk-sc-fonts \
    google-noto-sans-symbols2-fonts \
    google-roboto-fonts \
    google-roboto-slab-fonts

mkdir -p "$fonts_dir"
tmp=$(mktemp -d)
trap 'rm -rf "$tmp"' EXIT

# Icon glyphs. No distro package ships these. The URL floats on the latest
# release, so check the member names rather than trusting them.
curl -fsSL "$nf_url" -o "$tmp/nerd.tar.xz"
for member in SymbolsNerdFont-Regular.ttf SymbolsNerdFontMono-Regular.ttf; do
    tar -tJf "$tmp/nerd.tar.xz" "$member" >/dev/null 2>&1 || {
        echo "nerd-fonts release no longer contains $member; check $nf_url" >&2
        exit 1
    }
done
tar -xJf "$tmp/nerd.tar.xz" -C "$fonts_dir" \
    SymbolsNerdFont-Regular.ttf \
    SymbolsNerdFontMono-Regular.ttf

# Ubuntu Mono is the monospace .config/fontconfig names first, and is not
# packaged for this distro either.
curl -fsSL "$ubuntu_url" -o "$tmp/ubuntu.zip"
python3 - "$tmp/ubuntu.zip" "$fonts_dir" <<'PY'
import pathlib, sys, zipfile
src, dst = sys.argv[1], pathlib.Path(sys.argv[2])
with zipfile.ZipFile(src) as z:
    found = [n for n in z.namelist()
             if "UbuntuMono-" in n and n.endswith(".ttf")]
    if not found:
        sys.exit(f"no UbuntuMono faces in {src}; the pinned asset may have moved")
    for n in found:
        (dst / pathlib.Path(n).name).write_bytes(z.read(n))
PY

sudo install -Dm644 "$here/70-noto-cjk-sc-prefer.conf" "$conf_dst"
fc-cache -f

echo
failed=0
check() {
    local label=$1 pattern=$2 got
    got=$(fc-match "$pattern" --format '%{family}')
    if [[ $got == "$3" ]]; then
        printf '  %-28s %s\n' "$label" "$got"
    else
        printf '  %-28s %s  (expected %s)\n' "$label" "$got" "$3"
        failed=1
    fi
}
check 'sans-serif'            'sans-serif'      'Roboto'
check 'serif'                 'serif'           'Roboto Slab'
check 'monospace'             'monospace'       'Ubuntu Mono'
check 'han U+4E2D'            ':charset=4e2d'   'Noto Sans CJK SC'
check 'emoji U+1F680'         ':charset=1f680'  'Noto Color Emoji'
check 'symbols2 U+1F5C0'      ':charset=1f5c0'  'Noto Sans Symbols2'
check 'nerd icon U+E0B0'      ':charset=e0b0'   'Symbols Nerd Font'
echo
if (( failed )); then
    echo "font resolution is not what this script installs for; see README.md" >&2
    exit 1
fi
echo "restart anything already running: fonts are sampled once at startup."
