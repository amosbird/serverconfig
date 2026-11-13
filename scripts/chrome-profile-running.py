#!/usr/bin/env python3

import os
import pathlib
import re
import sys


def command_matches_profile(payload, profile):
    arguments = payload.split(b"\0")
    option = b"--user-data-dir"
    exact = option + b"=" + profile
    for index, argument in enumerate(arguments):
        if argument == exact:
            return True
        if argument == option and index + 1 < len(arguments) and arguments[index + 1] == profile:
            return True

    fields = [argument for argument in arguments if argument]
    if len(fields) != 1 or b" " not in fields[0]:
        return False
    pattern = re.compile(
        rb"(?:^|\s)--user-data-dir(?:=([^\s]+)|\s+([^\s]+))(?=\s|$)"
    )
    matches = pattern.findall(fields[0])
    if len(matches) != 1:
        return False
    return (matches[0][0] or matches[0][1]) == profile


def main():
    if len(sys.argv) != 3:
        print("usage: chrome-profile-running.py PROC_ROOT PROFILE", file=sys.stderr)
        return 2
    root = pathlib.Path(sys.argv[1])
    profile = os.fsencode(sys.argv[2])
    try:
        processes = list(root.iterdir())
    except OSError as error:
        print(f"chrome profile process scan failed: {error}", file=sys.stderr)
        return 2
    for process in processes:
        if not process.name.isdigit():
            continue
        try:
            payload = (process / "cmdline").read_bytes()
        except OSError:
            continue
        if command_matches_profile(payload, profile):
            return 0
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
