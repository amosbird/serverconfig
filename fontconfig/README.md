# Server fonts

Runbook for a new TencentOS (or similar RHEL-like) host. **Do not run any of
this on the Arch desktop, and `restore.sh` never installs it.**

A fresh TencentOS image ships only DejaVu: Latin, box drawing, geometric
shapes, arrows. Everything else is missing, so anything that renders text
shows tofu for Chinese and a box for every icon glyph.

## Install

From a clone of this repo:

```bash
bash ~/git/serverconfig/fontconfig/install-server.sh
```

The script is idempotent and prints what each generic family resolved to.
What it does, if you would rather run it by hand:

```bash
# 1. CJK, emoji, extra Unicode, and the sans/serif that .config/fontconfig
#    names first (see "Why Roboto" below — these are not optional)
sudo dnf install -y \
    google-noto-cjk-fonts \
    google-noto-emoji-color-fonts \
    google-noto-sans-mono-cjk-sc-fonts \
    google-noto-sans-symbols2-fonts \
    google-roboto-fonts \
    google-roboto-slab-fonts

# 2. Icon glyphs and Ubuntu Mono. Neither is packaged for this distro.
#    ~/.local/share/fonts is a real directory here, not a restore.sh symlink.
mkdir -p ~/.local/share/fonts
curl -fsSLO https://github.com/ryanoasis/nerd-fonts/releases/latest/download/NerdFontsSymbolsOnly.tar.xz
tar -xJf NerdFontsSymbolsOnly.tar.xz -C ~/.local/share/fonts \
    SymbolsNerdFont-Regular.ttf SymbolsNerdFontMono-Regular.ttf
curl -fsSLO https://assets.ubuntu.com/v1/fad7939b-ubuntu-font-family-0.83.zip
unzip -j fad7939b-ubuntu-font-family-0.83.zip '*/UbuntuMono-*.ttf' \
    -d ~/.local/share/fonts

# 3. Prefer Simplified Chinese faces over the JP default.
sudo install -Dm644 ~/git/serverconfig/fontconfig/70-noto-cjk-sc-prefer.conf \
    /etc/fonts/conf.d/70-noto-cjk-sc-prefer.conf

fc-cache -f
```

Skip `google-noto-sans-symbols` (the non-2 package): DejaVu already covers
what it adds. `google-noto-cjk-fonts` is a metapackage that pulls the TTC
sans + serif faces.

Long-running programs sample the font set once at startup, so restart
anything that was already open before `fc-cache` ran.

## Why Roboto and Ubuntu Mono are not optional

`.config/fontconfig/fonts.conf` is shared with the Arch desktop and lists, for
each generic family, a real text face and then `Symbols Nerd Font` as a
fallback: Roboto for sans, Roboto Slab for serif, Ubuntu Mono for mono. On the
desktop those exist and Symbols Nerd Font stays where it belongs, at the back.

On a bare server none of them exist, so installing the Nerd Font promotes a
**symbols-only face with no letters in it** to the head of all three generic
families, and `fc-match sans-serif` starts naming a font that cannot render
text. Per-character fallback hides this in browsers, but anything that takes
the first match at face value renders nothing.

Installing Roboto, Roboto Slab and Ubuntu Mono is the fix. Overriding the order
from a server-local drop-in instead does not work: `mode="prepend"` inserts
before the matched element, not at the head, and a `binding="weak"` entry sorts
below the strongly bound Symbols Nerd Font no matter where it sits. Even done
correctly with `prepend_first` plus `binding="strong"`, forcing a text face to
the front pushes Symbols Nerd Font far enough down that Chromium stops finding
it. Satisfy the config's expectations rather than fighting them.

## Why the drop-in exists

`70-noto-cjk-sc-prefer.conf` is numbered 70 because the Noto packages drop
rules at 65/66, and `local.conf` is pulled in at 51 — anything earlier loses.
Those package rules only test `lang=zh-cn` / `zh-sg` and bind every regional
face to the generic families, so a request carrying no language resolves to
Noto Sans CJK **JP** and 直 骨 次 者 每 令 come out with Japanese shapes.
A request that does carry `zh-CN` already resolves to SC without this file.

It lives outside `.config/` — not in the `configs=()` allowlist, not under
`.local/share/`, not referenced by the `GUI=1` block — so no invocation of
`restore.sh` can put it on the desktop. That matters because Noto Sans CJK
also covers box drawing, geometric shapes and circled numbers, and placing it
ahead of Symbols Nerd Font on Arch would silently repaint kitty and prompt
symbols.

## Quick check

```bash
for f in sans-serif serif monospace; do fc-match "$f" --format "$f -> %{family}\n"; done
fc-match ':charset=4e2d'  # Noto Sans CJK SC, not JP
fc-match ':charset=e0b0'  # Symbols Nerd Font
fc-match ':charset=1f680' # Noto Color Emoji
fc-match ':charset=1f5c0' # Noto Sans Symbols2
```

Expected:

| request | font |
|---|---|
| `sans-serif` / `serif` / `monospace` | Roboto / Roboto Slab / Ubuntu Mono |
| Han | Noto Sans CJK SC |
| emoji | Noto Color Emoji |
| `U+E0B0` powerline, `U+E700` devicons, `U+F000` font-awesome | Symbols Nerd Font |
| `U+1F5C0` folder / document symbols | Noto Sans Symbols2 |
| `U+2500` box drawing, `U+25A0` geometric, `U+2190` arrows | DejaVu |

## Two things fonts cannot fix

**Private Use Area icons in a browser.** Chromium does not fall back for PUA
codepoints, because they carry no Unicode script to search on. Installing
Symbols Nerd Font makes `font-family: 'Symbols Nerd Font'` work, and nothing
else: the identical codepoints in a `sans-serif` run still render as boxes.
Measured both ways on the same page, 4/4 versus 0/4. A page has to name the
font.

**Webfonts are not system fonts.** VS Code's icons are the `codicon` webfont
the page loads itself, so they are unaffected by everything here — they
already worked on a host with nothing but DejaVu installed.

`fc-match` only reports what fontconfig would pick, and a glyph no font covers
is still reported under some family name. To prove a character actually
rendered, drive `CSS.getPlatformFontsForNode` over CDP and rasterise each
character against that font's `.notdef` box.
