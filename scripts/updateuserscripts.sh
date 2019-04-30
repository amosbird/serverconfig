#!/usr/bin/env bash

urls=('https://raw.githubusercontent.com/ParticleCore/Iridium/master/src/Userscript/Iridium.user.js')
d=/home/amos/.local/share/qutebrowser/greasemonkey

for url in "${urls[@]}"; do
    (cd "$d" && curl -O "$url")
done
