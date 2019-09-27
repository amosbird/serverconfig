#!/usr/bin/env bash

p=$(equery which "$1")
if test -n "$p"
then
    trim=${p%/*/*/*};
    cd "$trim" || exit 1
    if test -e "$(dirname "../localrepo/${p#$trim/}")"
    then
        echo "localrepo already has it!"
        exit 1
    fi
    rsync -aR "${p#$trim/}" ../localrepo/
    e "$PWD/../localrepo/${p#$trim/}"
else
    exit 1
fi
