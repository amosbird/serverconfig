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

```bash
sudo install -Dm644 fontconfig/70-noto-cjk-sc-prefer.conf \
    /etc/fonts/conf.d/70-noto-cjk-sc-prefer.conf
fc-cache -f
```

Verify with the faces Chromium actually picked, not `fc-match`, which does not
predict its per-character fallback:

```bash
# in a page with no lang attribute
CSS.getPlatformFontsForNode   # via CDP on the terminal-browser instance
```
