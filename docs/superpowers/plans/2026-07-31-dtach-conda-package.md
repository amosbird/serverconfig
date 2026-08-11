# dtach Conda Package Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Build, publish, install, and restore the `amosbird/dtach` fork as a Linux conda package.

**Architecture:** A standalone `dtach-feedstock` builds from the fork's master tarball. The existing GitHub Releases channel carries the package and updated flat/compressed repodata; serverconfig names the package during first-time mambatools creation.

**Tech Stack:** Autoconf, Make, conda-build/rattler-build recipe v1, micromamba, GitHub Releases, Python 3.

---

### Task 1: Create the dtach feedstock

**Files:**
- Create: `~/git/dtach-feedstock/recipe/recipe.yaml`
- Create: `~/git/dtach-feedstock/recipe/build.sh`

- [ ] **Step 1: Write the recipe with a command test**

```yaml
package:
  name: dtach
  version: "0.9"

source:
  - url: https://github.com/amosbird/dtach/archive/refs/heads/master.tar.gz

build:
  number: 0
  skip: win

requirements:
  build:
    - ${{ compiler('c') }}
    - autoconf
    - make

tests:
  - script:
      - test -x "$PREFIX/bin/dtach"
      - "$PREFIX/bin/dtach" -h 2>&1 | grep -q '^Usage:'

about:
  homepage: https://github.com/amosbird/dtach
  license: GPL-2.0-or-later
  summary: Amos Bird's dtach fork
  license_file: COPYING
```

- [ ] **Step 2: Write the build script**

```bash
set -x
autoreconf -fi
./configure --prefix="$PREFIX"
make -j"${CPU_COUNT}"
make install
```

- [ ] **Step 3: Render the recipe and confirm it fails only if metadata is invalid**

Run:
```bash
conda build recipe/ --output -c conda-forge \
  --variant-config-files ~/git/emacs-feedstock/recipe/variants.yaml
```
Expected: prints a `dtach-0.9-*.conda` output path.

### Task 2: Build and locally verify the package

**Files:**
- Output: `~/.local/share/mamba/envs/emacs-build/conda-bld/linux-64/dtach-0.9-*.conda`

- [ ] **Step 1: Build the package**

Run:
```bash
conda build recipe/ --no-test -c conda-forge \
  --variant-config-files ~/git/emacs-feedstock/recipe/variants.yaml
```
Expected: `1 succeeded, 0 failed`.

- [ ] **Step 2: Install into a clean temporary prefix**

Run:
```bash
micromamba create -y -n dtach-test -c conda-forge /path/to/dtach-0.9-*.conda
```
Expected: transaction succeeds.

- [ ] **Step 3: Verify the installed binary**

Run:
```bash
micromamba run -n dtach-test dtach -h 2>&1 | grep '^Usage:'
```
Expected: a usage line and grep exit 0.

### Task 3: Publish through the channel

**Files:**
- Modify: `~/git/conda-channel/linux-64/repodata.json`
- Regenerate: `~/git/conda-channel/linux-64/repodata.json.zst`

- [ ] **Step 1: Update repodata**

Run:
```bash
cd ~/git/conda-channel
python3 update_repodata.py /path/to/dtach-0.9-*.conda
zstd -f linux-64/repodata.json -o linux-64/repodata.json.zst
```
Expected: reports `Added: dtach-0.9-...conda`.

- [ ] **Step 2: Upload package and replace indexes**

Run:
```bash
gh release upload linux-64 /path/to/dtach-0.9-*.conda --repo amosbird/conda-channel
gh release delete-asset linux-64 repodata.json --yes --repo amosbird/conda-channel
gh release delete-asset linux-64 repodata.json.zst --yes --repo amosbird/conda-channel
gh release upload linux-64 linux-64/repodata.json linux-64/repodata.json.zst \
  --repo amosbird/conda-channel
```
Expected: every command exits 0.

- [ ] **Step 3: Verify remote metadata**

Run:
```bash
curl -fsSL https://github.com/amosbird/conda-channel/releases/download/linux-64/repodata.json \
  | python3 -c 'import json,sys; assert any(v["name"] == "dtach" for v in json.load(sys.stdin)["packages.conda"].values())'
```
Expected: exit 0.

### Task 4: Remote install and serverconfig integration

**Files:**
- Modify: `restore.sh`
- Modify: `.config/skills/conda-channel.md`

- [ ] **Step 1: Install only from remote channels in a clean prefix**

Run:
```bash
micromamba create -y -n dtach-remote-test \
  -c https://github.com/amosbird/conda-channel/releases/download \
  -c https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud/conda-forge dtach
```
Expected: installs `dtach 0.9` from the GitHub channel.

- [ ] **Step 2: Verify the remote package**

Run:
```bash
micromamba run -n dtach-remote-test dtach -h 2>&1 | grep '^Usage:'
```
Expected: exit 0.

- [ ] **Step 3: Add `dtach` beside the other fork packages**

Change both installation commands from:
```bash
emacs tmux htop-vim
```
to:
```bash
emacs tmux htop-vim dtach
```

- [ ] **Step 4: Install into mambatools**

Run:
```bash
micromamba install -y -p ~/.mambatools \
  -c https://github.com/amosbird/conda-channel/releases/download \
  -c https://mirrors.tuna.tsinghua.edu.cn/anaconda/cloud/conda-forge dtach
```
Expected: transaction succeeds.

- [ ] **Step 5: Verify mambatools installation**

Run:
```bash
~/.mambatools/bin/dtach -h 2>&1 | grep '^Usage:'
```
Expected: exit 0.

- [ ] **Step 6: Commit serverconfig changes**

```bash
git add restore.sh .config/skills/conda-channel.md
git commit -m "Add dtach to mambatools bootstrap"
```

### Task 5: Final verification

- [ ] **Step 1: Verify recipe output, remote metadata, and installed binary**

Run:
```bash
test -f ~/git/dtach-feedstock/recipe/recipe.yaml && \
curl -fsSL https://github.com/amosbird/conda-channel/releases/download/linux-64/repodata.json \
  | grep -q '"name": "dtach"' && \
~/.mambatools/bin/dtach -h 2>&1 | grep -q '^Usage:'
```
Expected: exit 0.
