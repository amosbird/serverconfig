#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

configs=(
    .globalrc
    .clang_complete
    .editorconfig
    .gdbinit
    .gdbinit.d
    .cgdb
    .docker
    .direnvrc
    .clang-format
    .ctags
    scripts
)

for config in "${configs[@]}"; do
    rm -rf "$HOME/$config"
    ln -s "$DIR"/"$config" "$HOME"/
done
