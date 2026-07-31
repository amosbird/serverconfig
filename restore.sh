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
    .lldbinit
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

if [[ ! -f "$HOME/.config/fish/fish_variables" ]]; then
    printf '# VERSION: 3.0\nSETUVAR __fish_initialized:4300\n' > "$HOME/.config/fish/fish_variables"
fi

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

ln -sf "/tmp/gentoo/emacs" $HOME/.emacs.d
ln -sf "$DIR/.abemacs" $HOME/

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

MISE_BIN="$HOME/.local/bin/mise"
export GITHUB_TOKEN=${GITHUB_TOKEN:-$(pass show github/api-token 2>/dev/null)}
if [[ -x "$MISE_BIN" ]]; then
    "$MISE_BIN" self-update -y
else
    mkdir -p "$HOME/.local/bin"
    curl -fsSL https://mise.run | MISE_INSTALL_PATH="$MISE_BIN" sh
fi
"$MISE_BIN" install -y
"$MISE_BIN" upgrade -y
"$MISE_BIN" prune -y

# Link mise-installed binaries directly to ~/.local/bin (bypass shims)
"$MISE_BIN" bin-paths | while read -r bin; do
    for f in "$bin"/*; do
        [[ -f "$f" && -x "$f" ]] && ln -sf "$f" "$HOME/.local/bin/"
    done
done

if [[ -n $GUI ]]; then
    update-desktop-database "$HOME/.local/share/applications"
    sudo cp "$DIR"/xkb/symbols/{us,pc,inet} /usr/share/X11/xkb/symbols/
    setxkbmap us
    sudo mkdir -p /etc/pacman.d/hooks
    sudo cp "$DIR"/xkb-restore.hook /etc/pacman.d/hooks/xkb-restore.hook

    # Network stack: iwd + systemd-networkd (see network/README.md).
    # Config only, except retiring the obsolete fallback preference updater below.
    # No link-managing service or tunnel is stopped, so restore.sh does not drop the connection.
    sudo mkdir -p /etc/iwd /etc/systemd/network /etc/systemd/system/wpa_supplicant@.service.d
    sudo cp "$DIR"/network/iwd/main.conf /etc/iwd/main.conf
    sudo cp "$DIR"/network/systemd-network/*.network /etc/systemd/network/
    sudo cp "$DIR"/network/systemd/wpa_supplicant@.service.d/override.conf \
        /etc/systemd/system/wpa_supplicant@.service.d/override.conf
    sudo cp "$DIR"/network/systemd/network-{reconfigure.path,reconfigure.service} \
        /etc/systemd/system/
    sudo cp "$DIR"/network/udev/90-wired-8021x.rules /etc/udev/rules.d/

    # Retiring fallback only stops its policy-preference updates; it does not alter links.
    if sudo test -e /etc/systemd/system/network-fallback.service; then
        sudo systemctl disable --now network-fallback.service
    fi
    sudo rm -f /etc/systemd/system/network-fallback.service
    sudo rm -rf /var/lib/network-fallback
    sudo systemctl daemon-reload
    # SmartDNS base config.
    sudo mkdir -p /etc/smartdns
    sudo cp "$DIR"/network/smartdns/smartdns.conf /etc/smartdns/
    # office.conf, dhcp-dns.conf and ioa-dns.conf are rewritten per link by
    # network-reconfigure; only seed them so smartdns can start before it runs.
    [[ -e /etc/smartdns/office.conf ]] ||
        echo '# Not on the office LAN' | sudo tee /etc/smartdns/office.conf >/dev/null
    [[ -e /etc/smartdns/dhcp-dns.conf ]] ||
        sudo cp "$DIR"/network/smartdns/dhcp-dns.conf /etc/smartdns/dhcp-dns.conf
    [[ -e /etc/smartdns/ioa-dns.conf ]] ||
        echo '# IOA tunnel is down' | sudo tee /etc/smartdns/ioa-dns.conf >/dev/null

    sudo cp "$DIR"/gpu-switch/gpu-switch.service /etc/systemd/system/
    sudo udevadm control --reload-rules
    sudo systemctl daemon-reload
    sudo systemctl enable gpu-switch.service
    sudo systemctl enable network-reconfigure.path

    # Setup kitty desktop-ui portal (replaces xdg-desktop-portal-termfilechooser)
    kitten desktop-ui enable-portal
fi

echo 'Restored!'
