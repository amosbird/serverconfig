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

# Icon glyphs. No distro package ships these.
curl -fsSL "$nf_url" -o "$tmp/nerd.tar.xz"
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
    for n in z.namelist():
        if "UbuntuMono-" in n and n.endswith(".ttf"):
            (dst / pathlib.Path(n).name).write_bytes(z.read(n))
PY

sudo install -Dm644 "$here/70-noto-cjk-sc-prefer.conf" "$conf_dst"
fc-cache -f

echo
printf '%-18s %s\n' \
    'sans-serif' "$(fc-match sans-serif --format '%{family}')" \
    'serif' "$(fc-match serif --format '%{family}')" \
    'monospace' "$(fc-match monospace --format '%{family}')" \
    'han' "$(fc-match :charset=4e2d --format '%{family}')" \
    'emoji' "$(fc-match :charset=1f680 --format '%{family}')" \
    'nerd icon' "$(fc-match :charset=e0b0 --format '%{family}')"
echo
echo "expected: Roboto / Roboto Slab / Ubuntu Mono / Noto Sans CJK SC /"
echo "          Noto Color Emoji / Symbols Nerd Font"
echo "restart anything already running: fonts are sampled once at startup."
