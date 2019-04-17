#!/usr/bin/env bash

mkdir -p ~/gentoo/usr/local/portage/$1/$2
cp $3 ~/gentoo/usr/local/portage/$1/$2/
pushd ~/gentoo/usr/local/portage/$1/$2
repoman manifest
popd
