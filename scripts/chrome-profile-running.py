#!/usr/bin/env -S python3 -B -S

import os
import sys

_PACKED_RE = None


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
    if len(fields) != 1 or b" " not in fields[0] or option not in fields[0]:
        return False
    global _PACKED_RE
    if _PACKED_RE is None:
        import re
        _PACKED_RE = re.compile(
            rb"(?:^|\s)--user-data-dir(?:=([^\s]+)|\s+([^\s]+))(?=\s|$)"
        )
    matches = _PACKED_RE.findall(fields[0])
    if len(matches) != 1:
        return False
    return (matches[0][0] or matches[0][1]) == profile


def main():
    if len(sys.argv) != 3:
        print("usage: chrome-profile-running.py PROC_ROOT PROFILE", file=sys.stderr)
        return 2
    root = sys.argv[1]
    profile = os.fsencode(sys.argv[2])
    try:
        entries = list(os.scandir(root))
    except OSError as error:
        print(f"chrome profile process scan failed: {error}", file=sys.stderr)
        return 2
    for entry in entries:
        if not entry.name.isdigit():
            continue
        try:
            with open(os.path.join(entry.path, "cmdline"), "rb") as stream:
                payload = stream.read()
        except OSError:
            continue
        if command_matches_profile(payload, profile):
            return 0
    return 1


if __name__ == "__main__":
    raise SystemExit(main())
