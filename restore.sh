#!/usr/bin/env bash

set -e
set -x

cd $HOME

mkdir -p $HOME/.config

DIR="git/serverconfig"

mkdir -p $HOME/.lc
ln -sf "../$DIR/lc/config.json" $HOME/.lc/

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
    .xsettingsd
    .proxychains
    .ideavimrc
    scripts
)

for config in "${configs[@]}"; do
    rm -rf "$HOME/$config"
    ln -sf "$DIR/$config" $HOME/
done

for dotconfig in "$DIR/.config/"*; do
    rm -rf "$HOME/.config/$(basename "$dotconfig")"
    ln -sf "../$dotconfig" $HOME/.config/
done

mkdir -p $HOME/.local/share/

ln -sf /tmp/gentoo/usr/share/grc $HOME/.local/share/

for share in "$DIR/.local/share/"*; do
    rm -rf "$HOME/.local/share/$(basename "$share")"
    ln -sf "../../$share" $HOME/.local/share/
done

rm -rf $HOME/.tmux
rm -rf $HOME/.tmux.conf
ln -sf "$DIR/.tmux" $HOME/
ln -sf "$DIR/.tmux/.tmux.conf" $HOME/


lesskey "$DIR/lesskey"

mkdir -p -m 700 $HOME/.ssh
cp "$DIR"/id_rsa.pub $HOME/.ssh/

if [[ $# == 0 ]]; then
    if [ ! -d $HOME/.password-store/.git ]; then
        rm -rf $HOME/.password-store
        git clone git@github.com:amosbird/pass-store $HOME/.password-store
    fi
    (cd $HOME/.password-store && git pull)

    gpg --keyserver keyserver.ubuntu.com --recv-keys 80D430DCBECFEDB4
    echo -e "5\ny\n" | gpg --command-fd 0 --expert --edit-key 80D430DCBECFEDB4 trust
    gpg --keyserver keyserver.ubuntu.com --recv-keys C3BFA922206F41DA
    echo -e "5\ny\n" | gpg --command-fd 0 --expert --edit-key C3BFA922206F41DA trust
fi

if [[ -n $GUI ]]; then
    update-desktop-database "$HOME/.local/share/applications"
    sudo cp -r "$DIR"/xkb/* /usr/share/X11/xkb/symbols/
    setxkbmap us
fi

echo 'Restored!'
