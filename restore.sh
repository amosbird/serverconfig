#!/usr/bin/env bash

mkdir -p ~/.config

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

mkdir -p ~/.lc
ln -sf "$DIR"/lc/config.json "$HOME"/.lc/

configs=(
    .globalrc
    .clang_complete
    .editorconfig
    .gdbinit
    .gdbinit.d
    .cgdb
    .docker
    .jelly-conky
    .direnvrc
    .clang-format
    .ctags
    .gitconfig
    .Xresources
    .xprofile
    .xinitrc
    .gtkrc-2.0
    .stalonetrayrc
    .bashrc
    .perltidyrc
    .wgetrc
    .bash_profile
    .chisels
    .gitignore_global
    .fakevimrc
    .terminfo
    scripts
)

for config in "${configs[@]}"; do
    rm -rf "$HOME/$config"
    ln -sf "$DIR"/"$config" "$HOME"/
done

for dotconfig in $DIR/.config/*; do
    rm -rf "$HOME/.config/$(basename "$dotconfig")"
    ln -sf "$dotconfig" "$HOME/.config/"
done

rm -rf "$HOME/.tmux"
rm -rf "$HOME/.tmux.conf"
ln -sf "$DIR/.tmux" "$HOME"/
ln -sf "$DIR/.tmux/.tmux.conf" "$HOME"/

sudo cp -r "$DIR"/xkb/* /usr/share/X11/xkb/symbols/

setxkbmap us
