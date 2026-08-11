# dtach Conda Package Design

## Goal

Package the `amosbird/dtach` fork as a Linux conda package, publish it through the existing GitHub Releases channel, install it into `~/.mambatools`, and include it in new-machine restoration.

## Architecture

Create a focused `~/git/dtach-feedstock` containing only a rattler/conda recipe and build script. The recipe fetches the fork's master archive, builds version 0.9 using Autoconf and the conda C toolchain, and installs `dtach` under the target prefix.

The existing `amosbird/conda-channel` `linux-64` release remains the distribution point. Its `repodata.json` and compressed `repodata.json.zst` will describe the new package alongside Emacs, tmux, and htop-vim.

## Build and dependencies

`dtach` only needs a C compiler and libc. Build dependencies are the conda C compiler, Autoconf, and Make. No runtime dependencies are declared beyond the platform runtime inferred by conda tooling.

The build sequence is:

1. `autoreconf -fi`
2. `./configure --prefix="$PREFIX"`
3. `make -j"$CPU_COUNT"`
4. `make install`

## Verification

The recipe includes a runnable command test for the installed binary. After publication, a fresh temporary micromamba environment installs `dtach` using only the remote GitHub Releases channel plus conda-forge. The installed binary is then invoked to verify it loads and reports expected usage/version behavior.

## Restore integration

Add `dtach` to the package list in `restore.sh` and to the channel-management skill's new-machine command. Existing `~/.mambatools` installations are updated explicitly once; repeated `restore.sh` runs continue to skip package installation when the environment already exists.

## Scope

Linux `linux-64` only. No macOS, Windows, CI, or additional channel automation is introduced.
