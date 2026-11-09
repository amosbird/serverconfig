#!/usr/bin/env python3

import argparse
import json
import re
import subprocess
import time

PROPERTY_FIELDS = {
    "WM_WINDOW_ROLE": "wm_role",
    "_ABUSH_REMOTE_ID": "remote_id",
    "_ABUSH_WORKSPACE_ID": "workspace_id",
    "_ABUSH_WINDOW_GENERATION": "generation",
    "_ABUSH_CONTROL_STATE": "control_state",
}
PROPERTIES = [*PROPERTY_FIELDS, "WM_CLASS", "_NET_WM_NAME"]


def run(command):
    try:
        return subprocess.run(command, capture_output=True, text=True, check=False)
    except FileNotFoundError as error:
        raise RuntimeError(f"required command is unavailable: {command[0]}") from error


def focused():
    result = run(["xprop", "-root", "_NET_ACTIVE_WINDOW"])
    if result.returncode != 0:
        raise RuntimeError(f"xprop failed: {result.stderr.strip() or 'unknown error'}")
    match = re.search(r"window id # (0x[0-9a-fA-F]+)", result.stdout)
    return match.group(1).lower() if match else None


def candidates():
    result = run(["xwininfo", "-root", "-tree"])
    if result.returncode != 0:
        raise RuntimeError(f"xwininfo failed: {result.stderr.strip() or 'unknown error'}")
    for line in result.stdout.splitlines():
        fields = line.strip().split(maxsplit=1)
        if fields and fields[0].startswith("0x"):
            yield fields[0]


def parse_properties(raw):
    parsed = {}
    for line in raw.splitlines():
        if "=" not in line or "not found" in line:
            continue
        name, value = (part.strip() for part in line.split("=", 1))
        name = name.split("(", 1)[0].strip()
        strings = re.findall(r'"((?:[^"\\]|\\.)*)"', value)
        decoded = [bytes(item, "utf-8").decode("unicode_escape") for item in strings]
        if name in PROPERTY_FIELDS and decoded:
            parsed[PROPERTY_FIELDS[name]] = decoded[0]
        elif name == "WM_CLASS" and decoded:
            parsed["wm_class"] = decoded
        elif name == "_NET_WM_NAME" and decoded:
            parsed["title"] = decoded[0]
    return parsed


def properties(xid):
    result = run(["xprop", "-id", xid, *PROPERTIES])
    if result.returncode != 0:
        raise RuntimeError(f"xprop failed for {xid}: {result.stderr.strip() or 'unknown error'}")
    return parse_properties(result.stdout), result.stdout


def acceptance(timeout):
    before = focused()
    deadline = time.monotonic() + timeout
    found = None
    while time.monotonic() < deadline:
        for xid in candidates():
            values, raw = properties(xid)
            if values.get("workspace_id"):
                found = (xid, values, raw)
                break
        if found:
            break
        time.sleep(0.25)
    after = focused()
    expected = set(PROPERTY_FIELDS.values())
    values = found[1] if found else {}
    return {
        "remote_available": found is not None,
        "instruction": None if found else
            "Connect an Abush remote, click the extension action, then rerun this script.",
        "focused_before": before,
        "focused_after": after,
        "focus_preserved": before == after,
        "xid": found[0] if found else None,
        **{field: values.get(field) for field in PROPERTY_FIELDS.values()},
        "wm_class": values.get("wm_class"),
        "title": values.get("title"),
        "xprop_raw": found[2] if found else None,
        "qtile_attrs_visible": expected.issubset(values),
        "closed": False,
    }


def main():
    parser = argparse.ArgumentParser(description="Accept an Abush Browser X11 workspace")
    parser.add_argument("--timeout", type=float, default=15)
    parser.add_argument("--close", action="store_true")
    args = parser.parse_args()
    try:
        output = acceptance(args.timeout)
        if output["remote_available"] and args.close:
            result = run(["xdotool", "windowclose", output["xid"]])
            output["closed"] = result.returncode == 0
            if result.returncode != 0:
                output["close_error"] = result.stderr.strip() or "xdotool failed"
    except RuntimeError as error:
        output = {"error": str(error), "remote_available": False, "closed": False}
    print(json.dumps(output, separators=(",", ":")))


if __name__ == "__main__":
    main()
