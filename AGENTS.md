# AGENTS.md

## Repository Overview

Dotfiles/configuration repository — a deeply customized Linux environment built
over 10+ years. Everything is symlinked into `$HOME` via `restore.sh`. Edits
here are immediately live. This repo is the single source of truth.

**Not a software project** — no build system, no test suite, no CI. The "code"
is shell scripts, config files, and a small Go tool.

## Key Commands

| Action | Command |
|---|---|
| Deploy/install dotfiles | `bash restore.sh` (symlinks everything to `$HOME`) |
| Deploy with GUI extras | `GUI=1 bash restore.sh` (also installs udev, systemd, desktop entries) |
| Build search indexer | `cd tools/crush-search-index && go build -o ../../scripts/crush-search-index .` |
| Update system (Arch) | `scripts/update` (paru + VCS package rebuilds) |

There are no test, lint, or CI commands for this repository.

## Repository Structure

```
.                           # Dotfiles (symlinked to $HOME)
├── .bashrc, .bash_profile  # Bash config (minimal — fish is the real shell)
├── .gitconfig              # Git: rebase-on-pull, GPG signing, SSH URL rewrites
├── .editorconfig           # 4-space indent, LF line endings, 100 char max
├── .clang-format           # C++ style: 4-space, 100 cols, left pointer alignment
├── .gdbinit, .lldbinit     # Debugger configs
├── .abemacs/               # Doom Emacs config (init.el, lisp/, snippets)
├── .tmux/                  # tmux configs (8 variants: amos, crush, opencode, emacs, ...)
├── .config/
│   ├── crush/              # Crush AI agent config, skills, global AGENTS.md
│   ├── opencode/           # OpenCode AI agent config
│   ├── kitty/              # Terminal config (two modes: standalone + mux)
│   ├── fish/               # Fish shell config (primary shell)
│   ├── qtile/              # Window manager
│   ├── nvim/               # Neovim config
│   ├── helix/              # Helix editor config
│   ├── fcitx5/             # Chinese input method (rime)
│   ├── rofi/               # Launcher/menu
│   ├── skills/             # Shared AI skills (gh)
│   └── ...                 # Many more app configs
├── scripts/                # 400+ utility scripts (all in $PATH)
├── tools/
│   └── crush-search-index/ # Go tool: FTS5 search indexer for AI sessions
├── skills/
│   └── aidoc/              # AI doc-saving skill
├── ab-ebuild-repo/         # Custom Gentoo package repository
├── gdbprinters/            # GDB pretty printers (libcxx, ClickHouse)
├── gpu-switch/             # NVIDIA/Intel GPU switching (systemd service)
├── udev/                   # Custom udev rules and hwdb
├── xkb/                    # Custom keyboard layouts
└── restore.sh              # Deployment script (symlinks everything)
```

## Scripting Patterns & Conventions

### Shell scripts
- **Shebang**: Always `#!/usr/bin/env bash` (or `#!/usr/bin/env python3` for Python)
- **Wrapper pattern**: Scripts shadow system binaries (e.g., `scripts/ssh`,
  `scripts/docker`) by sitting earlier in `$PATH`, doing setup, then `exec`ing
  the real binary
- **Secret management**: All secrets loaded from `pass` (password-store) as env
  vars — never hardcoded
- **Environment detection**: Scripts branch on `$GUI` (desktop vs headless),
  `$session` (tmux session name), `$TERM`, etc.
- **No `set -e` by default** in most scripts; `update` uses `set -euo pipefail`

### Naming & style
- Script names are lowercase, short, no extensions (unless `.sh`/`.py`/`.pl`)
- Config files follow upstream conventions (no custom naming)
- **Indent**: 4 spaces everywhere (editorconfig), 2 for Emacs Lisp, tabs for Makefiles
- **Line endings**: Unix LF always
- **Max line length**: 100 characters

### Key tools in scripts
- **fzf**: Used extensively for interactive selection (sessions, windows, files)
- **SQLite + FTS5**: Full-text search for AI session history (with pinyin support)
- **OSC52**: Clipboard forwarding for remote sessions (kitty, tmux)
- **tmux**: Popup windows for interactive UIs (`tmux display-popup`)
- **pass**: GPG-encrypted password store for all secrets

## AI Agent Architecture

Two AI agent setups coexist in this repo:

### Crush
- Config: `.config/crush/crush.json`
- Global instructions: `.config/crush/AGENTS.md`
- Default permissions: **read-only** (`view`, `ls`, `grep` only)
- Models proxied through local `copilot-proxy` at `127.0.0.1:8787`
- Skills: `gh` (GitHub CLI)
- Session search: `scripts/crush-search` (fzf + FTS5 + pinyin)
- Tmux integration: dedicated tmux server at `/tmp/tmux-crush`

### OpenCode
- Config: `.config/opencode/opencode.json`

## Keybinding Philosophy

A consistent Emacs + Vim hybrid runs through the entire setup:
- **tmux prefix**: `C-x` (Emacs-style)
- **Kitty mux prefix**: `Ctrl+x>` (Emacs-style)
- **Modal editing**: Enabled in Emacs, Neovim, Helix, IdeaVim, FakeVim
- **Navigation**: `Alt+h/j/k/l` for pane/window movement (everywhere)
- **Window switching**: `Alt+1..8` across tmux, kitty, and Crush tmux
- **Prompt anchor**: `❯` symbol used for scrollback search/jump

## Environment Details

- **OS**: Arch Linux (host) + Gentoo Prefix at `/tmp/gentoo` (portable dev env)
- **Primary editor**: Emacs (Doom), with Neovim and Helix as alternates
- **Primary shell**: Fish (bash is `$SHELL` for compatibility, but fish is interactive)
- **Terminal**: Kitty (two modes: standalone and multiplexer)
- **Window manager**: Qtile (tiling, Python-configured)
- **Input method**: fcitx5 + rime (Chinese) — per-pane state tracked in tmux/kitty
- **Dev work**: Primarily ClickHouse on remote machines via SSH
- **Git**: Rebase-on-pull, GPG-signed commits, key `80D430DCBECFEDB4`

## Important Gotchas

1. **Symlink-based deployment**: All files here are symlinked to `$HOME`. Edits
   are live immediately — there is no "deploy" step for changes.
2. **Scripts shadow system binaries**: `scripts/ssh`, `scripts/docker`,
   `scripts/nnn` etc. are wrappers that `exec` the real binary. Don't confuse
   them with the actual tools.
3. **Two tmux servers**: The main tmux session and Crush's tmux are on separate
   sockets (`/tmp/tmux-*`). They don't share windows or state.
4. **Gentoo Prefix**: The `.bash_profile` auto-enters Gentoo Prefix on remote
   SSH connections (detected by `TERM=vt100`). This is intentional.
5. **Pass for secrets**: Never hardcode secrets. Use `pass show <key>` and
   inject via env vars, following the pattern in `scripts/crush` and
   `scripts/opencode`.
6. **fcitx5 state**: Input method state is tracked per-pane in tmux via hooks.
   If editing tmux config, preserve the `fcitx5-tmux-hook` integration.
7. **No tests**: This is a config repo. Validation is manual — test changes by
   running the affected tool/script directly.
