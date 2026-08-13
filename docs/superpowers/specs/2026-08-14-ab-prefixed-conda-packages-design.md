# AB-prefixed leaf conda packages design

## Goal

Rename the four personal leaf packages so their package identities cannot collide with packages from conda-forge:

- `emacs` → `ab-emacs`
- `tmux` → `ab-tmux`
- `htop-vim` → `ab-htop-vim`
- `dtach` → `ab-dtach`

Installed executables and payload paths remain unchanged (`bin/emacs`, `bin/tmux`, `bin/htop`, and `bin/dtach`).

## Package identity

Each feedstock changes only its top-level package name and increments its build number. The packages do not provide, constrain, depend on, or otherwise alias their old names. They are leaf packages selected explicitly by the workstation restore configuration.

This prevents `micromamba update --all` from replacing the custom `dtach` terminal tool with conda-forge's unrelated Python package or replacing the custom tmux fork with conda-forge tmux.

## Restore migration

`restore.sh` and the personal channel documentation install the four `ab-*` names. The existing local environment is migrated in one transaction by removing the old package records and installing the new package records. Since corresponding old and new packages own the same payload paths, old packages must be removed before or in the same explicit migration workflow rather than left co-installed.

## Validation

For each rebuilt package:

1. Install the exact artifact into a clean prefix with dependencies.
2. Confirm the expected executable exists and resolves shared libraries from the prefix.
3. Run the existing relocation scanner.
4. Run `micromamba update --all` using conda-forge alone and confirm all four `ab-*` package records and binaries remain installed.
5. Exercise remove and reinstall behavior.

Emacs additionally retains its relocation and native-compilation checks across differently sized prefixes.

## Publication

Regenerate `linux-64/repodata.json` and its zstd form with the four new package records. Upload the new artifacts, remove the old package records and old Release assets, then migrate and verify `/data/home/amos/.mambatools` from the published channel.

## Scope

No executable renaming, compatibility metapackages, dependency aliases, upstream source changes, or unrelated feedstock refactoring are included.
