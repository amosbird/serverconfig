#!/usr/bin/env bash

git submodule foreach --recursive rm -f .git/index.lock
git submodule foreach --recursive git reset --hard
git submodule foreach --recursive git clean -fdx
git submodule foreach --recursive git fetch --tags -f
git submodule sync --recursive
git submodule update --init --recursive
