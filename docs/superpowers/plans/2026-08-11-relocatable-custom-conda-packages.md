# Relocatable Custom Conda Packages Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rebuild Emacs, tmux, htop-vim, and dtach as conda packages that work after installation into arbitrary prefixes.

**Architecture:** Use conda prefix replacement for legitimate install-directory strings, `$ORIGIN` ELF RPATHs for libraries, and compiler prefix maps for debug data. Validate every artifact in two clean prefixes before replacing channel assets.

**Tech Stack:** conda-build/rattler-build recipes, micromamba, Autoconf, GCC/binutils, patchelf/readelf, GitHub Releases, Python 3.

---

### Task 1: Establish relocation checks

**Files:**
- Create: `~/git/conda-channel/check_relocatable.py`

- [ ] **Step 1: Write a scanner that fails on personal paths**

The script accepts a prefix and package filenames, scans package-owned regular files, and fails if they contain `/tmp/gentoo/home/amos` or `/data/home/amos`. It skips conda metadata because transaction records legitimately mention download/build locations.

- [ ] **Step 2: Run it against current installations and verify RED**

Run:
```bash
python3 ~/git/conda-channel/check_relocatable.py ~/.mambatools emacs tmux htop-vim dtach
```
Expected: FAIL with Emacs, tmux, or htop path findings.

### Task 2: Fix and rebuild tmux, htop-vim, and dtach

**Files:**
- Modify: `~/git/tmux-feedstock/recipe/recipe.yaml`
- Modify: `~/git/tmux-feedstock/recipe/build.sh`
- Modify: `~/git/htop-feedstock/recipe/recipe.yaml`
- Modify: `~/git/htop-feedstock/recipe/build.sh`
- Modify: `~/git/dtach-feedstock/recipe/recipe.yaml`
- Modify: `~/git/dtach-feedstock/recipe/build.sh`

- [ ] **Step 1: Increment each build number**

Set tmux, htop-vim, and dtach `build.number` to the next integer.

- [ ] **Step 2: Add compiler prefix maps**

Append these mappings without removing conda's existing flags:

```bash
export CFLAGS="${CFLAGS} -ffile-prefix-map=${SRC_DIR}=/usr/local/src/conda/${PKG_NAME}-${PKG_VERSION} -fdebug-prefix-map=${BUILD_PREFIX}=/usr/local/src/conda-build"
```

- [ ] **Step 3: Fix htop's runtime configuration directory**

Configure or minimally patch htop so it does not embed a personal installation prefix. Use a conda-relocatable prefix field if supported; otherwise derive the installed prefix from `/proc/self/exe` and append `/etc`.

- [ ] **Step 4: Build all three packages from clean work directories**

Run for each feedstock:
```bash
conda build purge
conda build recipe/ --no-test -c conda-forge \
  --variant-config-files ~/git/emacs-feedstock/recipe/variants.yaml
```
Expected: `1 succeeded, 0 failed`.

### Task 3: Fix and rebuild Emacs

**Files:**
- Modify: `~/git/emacs-feedstock/recipe/recipe.yaml`
- Modify: `~/git/emacs-feedstock/recipe/build.sh`

- [ ] **Step 1: Increment the Emacs build number**

Use a new build number so clients do not reuse the old artifact.

- [ ] **Step 2: Preserve placeholder prefixes through GCC JIT specs**

Generate specs using the conda placeholder paths in `$PREFIX`, do not rewrite them to a personal installation prefix, and keep the files visible to conda prefix detection rather than excluding all JIT files.

- [ ] **Step 3: Use relative RPATHs**

Ensure Emacs uses `$ORIGIN/../lib` and `$ORIGIN/../lib/emacs/jit/lib`; JIT libraries use their corresponding `$ORIGIN` paths. Do not embed `$PREFIX` as final RPATH.

- [ ] **Step 4: Normalize source/debug paths**

Add `-ffile-prefix-map` and `-fdebug-prefix-map` for source/build paths.

- [ ] **Step 5: Build from clean caches**

Run:
```bash
conda build purge-all
conda build recipe/ --no-test -c conda-forge \
  --variant-config-files recipe/variants.yaml
```
Expected: `1 succeeded, 0 failed`.

### Task 4: Two-prefix artifact verification

**Files:**
- Test only

- [ ] **Step 1: Install all artifacts into two prefixes**

Use fresh prefixes `/tmp/reloc-a` and `/data/home/amos/.cache/relocatable-conda-prefix-b`, installing exact local artifacts with dependencies from conda-forge.

- [ ] **Step 2: Verify commands and RPATHs**

Run package commands and require every custom executable's RPATH/RUNPATH to contain only `$ORIGIN`-relative entries.

- [ ] **Step 3: Run the personal-path scanner**

Run `check_relocatable.py` against both prefixes. Expected: PASS.

- [ ] **Step 4: Verify Emacs deeply**

For each prefix run:
```bash
emacs --batch -Q --eval '(unless (and (native-comp-available-p) (treesit-available-p) (json-available-p)) (kill-emacs 1))'
emacs --batch -Q --eval '(batch-native-compile t)' "$PREFIX/share/emacs/31.0.50/lisp/calculator.elc"
```
Also assert `invocation-directory`, `data-directory`, `doc-directory`, and `native-comp-eln-load-path` resolve beneath that prefix.

### Task 5: Publish and remotely verify

**Files:**
- Modify: `~/git/conda-channel/linux-64/repodata.json`
- Regenerate: `~/git/conda-channel/linux-64/repodata.json.zst`

- [ ] **Step 1: Update repodata with all four new artifacts**

Run `update_repodata.py` once per artifact, then regenerate the zstd index.

- [ ] **Step 2: Replace release assets**

Upload new package files, remove superseded package files and indexes, then upload both indexes.

- [ ] **Step 3: Clear index cache and install remotely into a clean prefix**

Use strict channel priority and `--repodata-ttl 0`. Assert all four packages come from the GitHub channel.

### Task 6: Reinstall under the current home

- [ ] **Step 1: Remove the four old packages from `/data/home/amos/.mambatools`**

- [ ] **Step 2: Clear package caches that could preserve old relocated files**

- [ ] **Step 3: Install all four packages from the remote channel**

Use strict channel priority and current repodata.

- [ ] **Step 4: Run final runtime, path, RPATH, and Emacs native-comp checks**

Expected: all pass, with no references to either personal home path in package-owned runtime files.
