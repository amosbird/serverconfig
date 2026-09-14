# fontconfig

Server-only font configuration. **Nothing here is deployed by `restore.sh`**, and
nothing here should ever be installed on the Arch desktop.

The desktop's font setup lives in `.config/fontconfig/fonts.conf`, which
`restore.sh` symlinks into `~/.config`. That file prefers WenQuanYi Micro Hei and
Symbols Nerd Font, and its priority order must stay untouched: Noto Sans CJK also
covers box drawing, geometric shapes and circled numbers, so inserting it ahead of
Symbols Nerd Font silently repaints symbols used across kitty and the shell prompt.

Keeping this directory outside `.config/` is what makes that impossible. It is not
in the `configs=()` allowlist, not under `.config/` or `.local/share/`, and not
referenced by the `GUI=1` block, so no invocation of `restore.sh` can reach it.

## Contents

- `70-noto-cjk-sc-prefer.conf` — makes Chinese resolve to the Simplified faces on
  hosts that only ship DejaVu plus Noto CJK. Needed because pages that declare no
  `lang` attribute otherwise render Chinese in Noto Sans CJK **JP**, giving Japanese
  shapes for characters like 直 骨 次 者 每 令. See the comments in the file for what
  is and isn't affected.

## Install (per server, by hand)

Fonts first. A bare TencentOS install ships only DejaVu, which covers Latin, box
drawing, geometric shapes and arrows, and nothing else:

```bash
# Chinese, plus colour emoji
sudo dnf install -y google-noto-cjk-fonts google-noto-emoji-color-fonts \
    google-noto-sans-mono-cjk-sc-fonts
# U+1F5C0 folder, U+1F5CE document, U+1F780 geometric, ... (symbols2 only;
# the non-2 package adds nothing DejaVu does not already cover)
sudo dnf install -y google-noto-sans-symbols2-fonts
```

Icon glyphs live in the Private Use Area and no distro package provides them, so
Nerd Fonts Symbols has to come from upstream. This is what `.config/fontconfig`
already expects: it names `Symbols Nerd Font` in all three generic families, and
maps the `SymbolsNFM` PostScript name for kitty. Installing the font is enough,
no config change:

```bash
curl -fsSLO https://github.com/ryanoasis/nerd-fonts/releases/latest/download/NerdFontsSymbolsOnly.tar.xz
mkdir -p ~/.local/share/fonts && tar -xJf NerdFontsSymbolsOnly.tar.xz \
    -C ~/.local/share/fonts SymbolsNerdFont-Regular.ttf SymbolsNerdFontMono-Regular.ttf
```

Then this directory's config, and one cache rebuild for all of the above:

```bash
sudo install -Dm644 fontconfig/70-noto-cjk-sc-prefer.conf \
    /etc/fonts/conf.d/70-noto-cjk-sc-prefer.conf
fc-cache -f
```

Note that `~/.local/share/fonts` is a real directory here, not one of the
`restore.sh` symlinks, so nothing above reaches the repo or the desktop.

## Verifying

`fc-match` does not predict Chromium's per-character fallback, and a glyph no
font covers is still reported under some family name, so neither one proves a
character rendered. Drive `CSS.getPlatformFontsForNode` over CDP against the
terminal-browser instance instead, and rasterise each character to compare it
against that font's `.notdef` box.

Coverage on a correctly set up host, by codepoint range:

| range | font |
|---|---|
| `U+E0B0` powerline, `U+E700` devicons, `U+F000` font-awesome | Symbols Nerd Font |
| `U+EA60` vscode codicons | `codicon`, a webfont the page loads itself |
| `U+2500` box drawing, `U+25A0` geometric, `U+2190` arrows | DejaVu, or Noto Sans CJK SC on a page declaring `lang="zh"` |
| `U+1F5C0` symbols | Noto Sans Symbols2 |
| emoji | Noto Color Emoji |
| Han | Noto Sans CJK SC |
