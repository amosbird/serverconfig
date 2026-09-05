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
    .xserverrc
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

for share in "$DIR/.local/share/"*; do
    rm -rf "$HOME/.local/share/$(basename "$share")"
    ln -sf "../../$share" $HOME/.local/share/
done

rm -rf $HOME/.tmux
rm -rf $HOME/.tmux.conf
ln -sf "$DIR/.tmux" $HOME/
ln -sf "$DIR/.tmux/.tmux.conf" $HOME/

ln -sf "$DIR/.abemacs" $HOME/

# Retire the old Gentoo Prefix Emacs checkout without touching a real directory.
if [[ -L "$HOME/.emacs.d" && ! -e "$HOME/.emacs.d" ]]; then
    rm "$HOME/.emacs.d"
fi

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

# Remove links left behind by pruned mise versions or an old home prefix.
for f in "$HOME/.local/bin"/*; do
    if [[ -L "$f" && ! -e "$f" ]]; then
        target=$(readlink "$f")
        if [[ "$target" == */.local/share/mise/installs/* ]]; then
            rm -f "$f"
        fi
    fi
done

# Link mise-installed binaries directly to ~/.local/bin (bypass shims)
"$MISE_BIN" bin-paths | while read -r bin; do
    for f in "$bin"/*; do
        if [[ -f "$f" && -x "$f" ]]; then
            ln -sf "$f" "$HOME/.local/bin/"
        fi
    done
done
ln -sf gopass "$HOME/.local/bin/pass"

"$HOME/scripts/update-mambatools"

if [[ -n $GUI ]]; then
    paru -S --needed --noconfirm touchegg
    "$HOME/scripts/package-external-links"
    sudo rm -f /etc/opt/chrome/policies/managed/extensions.json \
        /etc/chromium/policies/managed/extensions.json
    sudo install -Dm644 "$DIR/chromium/external-links/package/external-extension.json" \
        /opt/google/chrome/extensions/ahonjbfnmpjgjgaabbppofphidfomgcj.json
    install -Dm755 "$DIR/rofi-chrome/host/main.py" \
        "$HOME/.local/share/rofi-chrome/host/main.py"
    install -Dm644 "$DIR/rofi-chrome/io.github.amosbird.rofi.chrome.json" \
        "$HOME/.config/google-chrome-main/NativeMessagingHosts/io.github.amosbird.rofi.chrome.json"
    update-desktop-database "$HOME/.local/share/applications"
    sudo cp "$DIR"/xkb/symbols/{us,pc,inet} /usr/share/X11/xkb/symbols/
    sudo install -Dm644 "$DIR"/xorg/30-touchpad.conf \
        /etc/X11/xorg.conf.d/30-touchpad.conf
    sudo install -Dm644 "$DIR"/modprobe.d/psmouse.conf /etc/modprobe.d/psmouse.conf
    sudo install -Dm644 "$DIR"/tlp/tlp.conf /etc/tlp.conf
    setxkbmap us
    sudo mkdir -p /etc/pacman.d/hooks
    sudo cp "$DIR"/xkb-restore.hook /etc/pacman.d/hooks/xkb-restore.hook

    # Network stack: iwd + systemd-networkd (see network/README.md).
    # Install config now and enable link owners for the next boot without dropping this connection.
    sudo mkdir -p /etc/iwd /etc/systemd/network /etc/systemd/networkd.conf.d \
        /etc/systemd/system/wpa_supplicant@.service.d
    sudo cp "$DIR"/network/iwd/main.conf /etc/iwd/main.conf
    sudo cp "$DIR"/network/systemd-network/*.network /etc/systemd/network/
    sudo cp "$DIR"/network/systemd-network/*.link /etc/systemd/network/
    if sudo systemctl is-active --quiet systemd-networkd.service; then
        sudo networkctl reload
    fi
    sudo cp "$DIR"/network/systemd-networkd.conf.d/foreign-routing.conf \
        /etc/systemd/networkd.conf.d/foreign-routing.conf
    sudo cp "$DIR"/network/systemd/wpa_supplicant@.service.d/override.conf \
        /etc/systemd/system/wpa_supplicant@.service.d/override.conf
    sudo cp "$DIR"/network/systemd/network-{reconfigure.path,reconfigure.service} \
        /etc/systemd/system/
    sudo rm -f /etc/systemd/system/tailscaled.service.d/transport.conf
    sudo systemctl disable --now network-debug-pcap.service 2>/dev/null || true
    sudo rm -f /etc/systemd/system/network-debug-pcap.service
    sudo rm -rf /var/log/network-debug/ring
    sudo cp "$DIR"/network/iOA /usr/lib/iOA/bin/iOA
    sudo cp "$DIR"/network/udev/90-wired-8021x.rules /etc/udev/rules.d/

    sudo rm -f /var/lib/network-reconfigure/derp-ips \
        /var/lib/network-reconfigure/ioa-endpoints

    sudo systemctl daemon-reload
    # SmartDNS includes must exist before validating and atomically installing the base config.
    sudo mkdir -p /etc/smartdns
    [[ -e /etc/smartdns/office.conf ]] ||
        echo '# Not on the office LAN' | sudo tee /etc/smartdns/office.conf >/dev/null
    [[ -e /etc/smartdns/dhcp-dns.conf ]] ||
        sudo cp "$DIR"/network/smartdns/dhcp-dns.conf /etc/smartdns/dhcp-dns.conf
    sudo "$DIR"/network/smartdns-deploy.sh "$DIR"/network/smartdns/smartdns.conf

    sudo mkdir -p /etc/systemd/system/getty@tty1.service.d
    sudo cp "$DIR"/systemd/getty-autologin.conf \
        /etc/systemd/system/getty@tty1.service.d/autologin.conf
    sudo systemctl enable getty@tty1.service

    systemctl --user enable --now gcr-ssh-agent.socket

    sudo cp "$DIR"/gpu-switch/gpu-switch.service /etc/systemd/system/
    sudo udevadm control --reload-rules
    sudo systemctl daemon-reload
    sudo systemctl enable gpu-switch.service
    sudo systemctl disable --now libinput-gestures.service 2>/dev/null || true
    sudo install -Dm644 "$DIR/systemd/touchegg.service.d/override.conf" \
        /etc/systemd/system/touchegg.service.d/override.conf
    sudo systemctl daemon-reload
    sudo systemctl enable --now touchegg.service
    install -Dm644 "$DIR/systemd/audio-mute-led.service" \
        "$HOME/.config/systemd/user/audio-mute-led.service"
    rm -f "$HOME/.config/systemd/user/bluetooth-audio-default.service" \
        "$HOME/.config/systemd/user/bluetooth-sco-watchdog.service"
    systemctl --user daemon-reload
    systemctl --user enable --now audio-mute-led.service
    systemctl --user enable --now \
        pipewire.service pipewire-pulse.service wireplumber.service
    wpctl settings -d bluetooth.autoswitch-to-headset-profile
    wpctl settings -d bluetooth.use-persistent-storage
    wpctl settings -d bluetooth.profile-preference
    wpctl settings -d device.restore-profile
    wpctl settings -d linking.follow-default-target
    wpctl settings -d node.stream.restore-target
    wpctl settings -d node.restore-default-targets
    systemctl --user restart wireplumber.service
    sudo systemctl enable --now bluetooth.service
    sudo systemctl enable systemd-networkd.service iwd.service
    sudo systemctl enable network-reconfigure.path
fi

echo 'Restored!'
