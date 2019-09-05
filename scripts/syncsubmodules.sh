#!/usr/bin/env bash

git submodule foreach --recursive rm -f .git/index.lock
git submodule foreach --recursive git reset --hard
git submodule sync
git submodule update --init --recursive
