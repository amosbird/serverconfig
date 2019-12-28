#!/usr/bin/env bash

mkdir -p /home/amos/.config

DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"

mkdir -p /home/amos/.lc
ln -sf "$DIR"/lc/config.json /home/amos/.lc/

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
    .npmrc
    .prettierrc
    .bash_profile
    .chisels
    .gitignore_global
    .fakevimrc
    .terminfo
    scripts
)

for config in "${configs[@]}"; do
    rm -rf "/home/amos/$config"
    ln -sf "$DIR/$config" /home/amos/
done

for dotconfig in "$DIR/.config/"*; do
    rm -rf "/home/amos/.config/$(basename "$dotconfig")"
    ln -sf "$dotconfig" /home/amos/.config/
done

rm -rf /home/amos/.tmux
rm -rf /home/amos/.tmux.conf
ln -sf "$DIR/.tmux" /home/amos/
ln -sf "$DIR/.tmux/.tmux.conf" /home/amos/

mkdir -p /home/amos/.local/share/
ln -sf /home/amos/gentoo/usr/share/grc /home/amos/.local/share/

lesskey "$DIR/lesskey"

mkdir -p -m 700 /home/amos/.ssh
cp "$DIR"/id_rsa.pub /home/amos/.ssh/

if [[ $# == 0 ]]; then
    if [ ! -d /home/amos/.password-store/.git ]; then
        rm -rf /home/amos/.password-store
        git clone git@github.com:amosbird/pass-store /home/amos/.password-store
    fi
    (cd /home/amos/.password-store && git pull)

    sudo cp -r "$DIR"/xkb/* /usr/share/X11/xkb/symbols/

    setxkbmap us

    gpg --recv-keys 80D430DCBECFEDB4
    echo -e "5\ny\n" | gpg --command-fd 0 --expert --edit-key 80D430DCBECFEDB4 trust
    gpg --recv-keys C3BFA922206F41DA
    echo -e "5\ny\n" | gpg --command-fd 0 --expert --edit-key C3BFA922206F41DA trust
fi

echo 'Restored!'
