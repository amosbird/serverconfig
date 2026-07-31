# Conda Channel Management

Personal conda channel hosted via GitHub Releases at `amosbird/conda-channel`.

## Channel URL

```
https://github.com/amosbird/conda-channel/releases/download
```

## Structure

- Tag `linux-64` → release assets: `repodata.json` + all `.conda` packages
- Tag `noarch` → release assets: `repodata.json` (empty)

## Install on new machine

```bash
micromamba create -p ~/.mambatools \
  -c https://github.com/amosbird/conda-channel/releases/download \
  -c conda-forge \
  emacs tmux htop-vim \
  gcc gxx gdb cmake make ninja lld lldb \
  rust cargo-zigbuild \
  rust-std-aarch64-apple-darwin rust-std-x86_64-apple-darwin rust-std-x86_64-pc-windows-gnu \
  zig \
  clang_osx-64 cctools_osx-64 ld64_osx-64 'sdkroot_env_osx-64=15.5'
```

## Update a package

### 1. Build

```bash
eval "$(micromamba shell hook -s bash)" && micromamba activate emacs-build
cd ~/git/<package>-feedstock
conda build recipe/ --no-test -c conda-forge --variant-config-files recipe/variants.yaml
```

Output: `~/.local/share/mamba/envs/emacs-build/conda-bld/linux-64/<name>-<ver>-<hash>.conda`

### 2. Update repodata.json

```bash
cd ~/git/conda-channel
# Download current repodata
gh release download linux-64 --pattern repodata.json --dir . --repo amosbird/conda-channel --clobber

# Add new package entry
python3 update_repodata.py /path/to/<name>-<ver>-<hash>.conda

# If replacing an old version, the script removes the old entry automatically
```

### 3. Upload

```bash
# Upload new package
gh release upload linux-64 /path/to/<name>-<ver>-<hash>.conda --repo amosbird/conda-channel

# Delete old repodata.json and re-upload
gh release delete-asset linux-64 repodata.json --yes --repo amosbird/conda-channel
gh release upload linux-64 repodata.json --repo amosbird/conda-channel

# Optionally delete old package version
gh release delete-asset linux-64 <old-pkg>.conda --yes --repo amosbird/conda-channel
```

### 4. Verify

```bash
micromamba install -p ~/.mambatools \
  -c https://github.com/amosbird/conda-channel/releases/download \
  -c conda-forge \
  <package-name>
```

## Feedstock locations

| Package | Feedstock | Source repo |
|---------|-----------|-------------|
| emacs | `~/git/emacs-feedstock` | `amosbird/emacs` |
| tmux | `~/git/tmux-feedstock` | `amosbird/tmux` |
| htop-vim | `~/git/htop-feedstock` | `amosbird/htop-vim` |
