# AB-prefixed Conda Packages Implementation Plan

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Publish and locally install four collision-free leaf packages named `ab-emacs`, `ab-tmux`, `ab-htop-vim`, and `ab-dtach`.

**Architecture:** Rename only each conda package identity while preserving installed executable paths. Update restore configuration, rebuild exact artifacts, verify clean-prefix lifecycle and solver behavior, replace old Release records/assets, and migrate the live prefix.

**Tech Stack:** rattler-build recipe YAML, conda-build, micromamba, GitHub CLI, shell verification scripts

---

### Task 1: Rename the four feedstock packages

**Files:**
- Modify: `/data/home/amos/git/emacs-feedstock/recipe/recipe.yaml`
- Modify: `/data/home/amos/git/tmux-feedstock/recipe/recipe.yaml`
- Modify: `/data/home/amos/git/htop-feedstock/recipe/recipe.yaml`
- Modify: `/data/home/amos/git/dtach-feedstock/recipe/recipe.yaml`

- [ ] **Step 1: Write the failing metadata check**

Run a Python assertion that expects package names `ab-emacs`, `ab-tmux`, `ab-htop-vim`, and `ab-dtach` in their respective recipe files.

- [ ] **Step 2: Run the check and verify it fails on the old names**

Expected: assertion failure identifying the first recipe still using its old name.

- [ ] **Step 3: Change package names and increment build numbers**

Use these names and build numbers:

```text
ab-emacs: 8
ab-tmux: 2
ab-htop-vim: 3
ab-dtach: 2
```

Do not alter payload paths, dependencies, or source revisions.

- [ ] **Step 4: Re-run the metadata check**

Expected: all four assertions pass.

- [ ] **Step 5: Commit each feedstock independently**

```bash
git add recipe/recipe.yaml
git commit -m "build: rename package to ab-<name>"
```

### Task 2: Update workstation restoration

**Files:**
- Modify: `/data/home/amos/git/serverconfig/restore.sh`
- Modify: `/data/home/amos/git/serverconfig/.config/skills/conda-channel.md`

- [ ] **Step 1: Write a failing grep check**

Assert both files use all four `ab-*` package names and no install command lists the four old names.

- [ ] **Step 2: Run it and verify failure**

Expected: failure because `restore.sh` currently lists old package names.

- [ ] **Step 3: Replace old package identities**

Set the custom package list to:

```bash
ab-emacs ab-tmux ab-htop-vim ab-dtach
```

Keep executable references unchanged.

- [ ] **Step 4: Run `bash -n restore.sh` and the grep check**

Expected: both exit zero.

- [ ] **Step 5: Commit**

```bash
git add restore.sh .config/skills/conda-channel.md
git commit -m "config: install AB-prefixed conda packages"
```

### Task 3: Build the four artifacts

**Files:**
- Build outputs under `/tmp/ab-conda-build`

- [ ] **Step 1: Build each recipe in the existing build environment**

Build Emacs with its variant file and the other three with their existing recipe commands. Use `--no-test` only where the current recipes require external post-build integration checks.

- [ ] **Step 2: Assert exact artifact names exist**

Expected:

```text
ab-emacs-31.0.50-h2c165bd_8.conda
ab-tmux-3.5a-hf9bef72_2.conda
ab-htop-vim-2.2.0-hf9bef72_3.conda
ab-dtach-0.9-hf9bef72_2.conda
```

### Task 4: Verify package payload and lifecycle

**Files:**
- Existing check: `/data/home/amos/git/conda-channel/check_relocatable.py`
- Existing check: `/data/home/amos/git/emacs-feedstock/recipe/test-relocation.sh`

- [ ] **Step 1: Install exact artifacts into `/tmp/ab-package-test`**

Include dependencies from conda-forge.

- [ ] **Step 2: Check package records and binaries**

Assert all four `ab-*` records exist and these files are executable:

```text
bin/emacs
bin/tmux
bin/htop
bin/dtach
```

- [ ] **Step 3: Run relocation checks**

Run `check_relocatable.py` for all four new package identities and `test-relocation.sh` for Emacs.

- [ ] **Step 4: Run native compilation**

Compile `calculator.elc` with the packaged Emacs; expected exit zero.

- [ ] **Step 5: Run solver regression check**

Run `micromamba update --all -c conda-forge`, then assert all four `ab-*` records and binaries remain. This is the regression check for the package-name collision.

- [ ] **Step 6: Exercise remove/reinstall in isolated prefixes**

For each package, remove it, assert its owned binary disappears, reinstall the exact local artifact, and assert the binary returns.

### Task 5: Publish replacement channel records

**Files:**
- Modify: `/data/home/amos/git/conda-channel/linux-64/repodata.json`
- Regenerate: `/data/home/amos/git/conda-channel/linux-64/repodata.json.zst`

- [ ] **Step 1: Download current Release repodata**

Confirm the local starting metadata matches the published metadata.

- [ ] **Step 2: Add four new artifacts and remove four old records**

Use `update_repodata.py`, then assert repodata contains only the four `ab-*` names among these custom package families.

- [ ] **Step 3: Regenerate compressed repodata**

```bash
zstd -f linux-64/repodata.json -o linux-64/repodata.json.zst
```

- [ ] **Step 4: Replace GitHub Release assets**

Upload the four new artifacts and both repodata files. Delete old assets only after all clean-prefix tests pass.

- [ ] **Step 5: Verify remote assets and repodata**

Expected: new `ab-*` artifacts exist; old four artifacts and records do not.

### Task 6: Migrate and verify the live prefix

**Files:**
- Prefix: `/data/home/amos/.mambatools`

- [ ] **Step 1: Remove old package identities**

Remove `emacs`, `tmux`, `htop-vim`, and `dtach` without removing unrelated dependencies.

- [ ] **Step 2: Install exact published `ab-*` builds**

Use strict channel priority with the personal channel first and conda-forge second.

- [ ] **Step 3: Verify package records and executable ownership**

Assert old package records are absent, new package records are present, and all four binaries exist.

- [ ] **Step 4: Run final relocation and native-comp checks**

Run both existing relocation checks and native compilation against `/data/home/amos/.mambatools`.

- [ ] **Step 5: Run final update-all regression check**

Use conda-forge alone in dry-run and assert no action unlinks any `ab-*` package. Then run the normal restore install command in dry-run and expect “All requested packages already installed.”

- [ ] **Step 6: Report commit SHAs, artifact hashes, published assets, and installed builds**
