# Relocatable Custom Conda Packages Design

## Goal

Make the custom Emacs, tmux, htop-vim, and dtach conda packages independent of both the build machine path and the first installation prefix, then republish and reinstall them under the current home.

## Scope

The work covers these four Linux `linux-64` packages only:

- `emacs 31.0.50`
- `tmux 3.5a`
- `htop-vim 2.2.0`
- `dtach 0.9`

## Relocation model

Use conda's native prefix replacement for data that must contain the installation prefix, and `$ORIGIN`-relative ELF RPATHs for shared-library discovery. Do not introduce runtime wrappers.

Build paths embedded solely in debug information are normalized with compiler prefix-map flags. They may use `/usr/local/src/conda/...`, but must not contain `/tmp/gentoo/home/amos` or `/data/home/amos`.

## Package-specific handling

### Emacs

Emacs embeds installation directories in its executable, portable dump, Lisp files, native-comp configuration, and GCC JIT specs. The recipe must preserve these as conda-relocatable placeholders rather than shipping a package that was already relocated into a personal prefix.

The libgccjit subtree remains inside `lib/emacs/jit`. Its driver specs must use the conda build placeholder where relocation is supported, and installed Emacs must resolve native compilation inside its current prefix. The executable and JIT libraries retain `$ORIGIN`-relative RPATHs.

### tmux

Keep the existing `$ORIGIN/../lib` RPATH. Normalize compiler and source paths in debug data. No runtime prefix should be required.

### htop-vim

The configured system configuration directory currently embeds the build/install prefix. Configure it as a relocatable path or patch runtime lookup to derive the prefix from the executable. Prefer conda prefix replacement if the fixed field is safely replaceable; otherwise use an executable-relative lookup in the fork.

### dtach

Keep the existing `$ORIGIN/../lib` RPATH and normalize debug paths. dtach has no runtime data directory.

## Build isolation

Every release build starts from clean conda-build work and package caches. No package extracted or relocated into a previous home may be reused as the publication artifact.

Each recipe increments its build number so clients cannot reuse an older cache entry with the same filename and metadata.

## Verification

Each freshly built package is installed from its `.conda` artifact into two clean prefixes with different path lengths:

- `/tmp/reloc-a`
- `/data/home/amos/.cache/relocatable-conda-prefix-b`

Checks:

1. All package commands start successfully.
2. `readelf` reports only `$ORIGIN`-relative RPATH/RUNPATH entries.
3. Runtime package files do not contain `/tmp/gentoo/home/amos` or `/data/home/amos`.
4. Emacs reports paths beneath the active prefix and passes native-comp, tree-sitter, JSON, and batch native-compilation checks.
5. htop resolves its configuration directory under the active prefix or an explicitly supported user/system location, not a build prefix.
6. Remote channel installation selects the custom packages under strict channel priority.

## Publication

Update the `linux-64` GitHub Release with all four new package files. Regenerate and upload `repodata.json` and `repodata.json.zst`, removing superseded package assets and metadata entries. Finally reinstall all four packages into `/data/home/amos/.mambatools` from the remote channel and rerun the verification suite.
