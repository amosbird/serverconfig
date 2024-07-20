#!/usr/bin/env bash

# Ideally we should use clangd
if [ -e "$1/build/compile_commands.json" ]; then
    jq --raw-output '.[0].command' "$1/build/compile_commands.json" | sed 's/\(.*\) -o.*/\1/'
elif [ -n "$CXX" ]; then
    echo "$CXX $CXXFLAGS"
fi
