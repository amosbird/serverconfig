#!/usr/bin/env bash

mkdir -p ~/gentoo/usr/local/portage/"$1"/"$2"
cp "$3" ~/gentoo/usr/local/portage/"$1"/"$2"/
cd ~/gentoo/usr/local/portage/"$1"/"$2" && repoman manifest
