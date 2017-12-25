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
    .gitconfig
    .Xresources
    .xprofile
    scripts
)

for config in "${configs[@]}"; do
    rm -rf "$HOME/$config"
    ln -s "$DIR"/"$config" "$HOME"/
done

for dotconfig in `ls .config`; do
    rm -rf "$HOME/.config/$dotconfig"
    ln -s "$DIR/.config/$dotconfig" "$HOME/.config/$dotconfig"
done

rm -rf "$HOME/.tmux"
rm -rf "$HOME/.tmux.conf"
ln -s "$DIR/.tmux" $HOME/
ln -s "$DIR/.tmux/.tmux.conf" $HOME/

sudo cp -r $DIR/xkb/* /usr/share/X11/xkb/symbols/

setxkbmap us
