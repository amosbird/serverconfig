#!/usr/bin/env bash

# Ideally we should use clangd
jq --raw-output '.[0].command' "$1/build/compile_commands.json" | sed 's/\(.*\) -o.*/\1/'
