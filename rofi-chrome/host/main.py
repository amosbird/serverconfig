#!/usr/bin/env python3

import json
import re
import struct
import subprocess
import sys

MAX_MESSAGE_SIZE = 16 * 1024 * 1024


def read_exact(stream, size):
    data = bytearray()
    while len(data) < size:
        chunk = stream.read(size - len(data))
        if not chunk:
            raise EOFError("native message ended early")
        data.extend(chunk)
    return bytes(data)


def read_message():
    header = sys.stdin.buffer.read(4)
    if not header:
        return None
    if len(header) != 4:
        raise EOFError("native message header ended early")
    size = struct.unpack("<I", header)[0]
    if size > MAX_MESSAGE_SIZE:
        raise ValueError(f"native message is too large: {size} bytes")
    return json.loads(read_exact(sys.stdin.buffer, size).decode("utf-8"))


def send_message(message):
    payload = json.dumps(message).encode("utf-8")
    sys.stdout.buffer.write(struct.pack("<I", len(payload)))
    sys.stdout.buffer.write(payload)
    sys.stdout.buffer.flush()


def rofi_select(param):
    command = ["rofi", "-dmenu", *param.get("rofi-opts", [])]
    result = subprocess.run(
        command,
        input="\n".join(param["opts"]),
        text=True,
        stdout=subprocess.PIPE,
        check=False,
    )
    return result.returncode, result.stdout.rstrip("\n")


def blocklist_patterns():
    try:
        output = subprocess.run(
            ["rofi-browser-blocklist.sh"],
            text=True,
            stdout=subprocess.PIPE,
            stderr=subprocess.DEVNULL,
            check=True,
        ).stdout
        # The shared blocklist prints "label ::: regex"; only the regex is relevant here.
        return [re.compile(line.rsplit(" ::: ", 1)[-1]) for line in output.splitlines() if line]
    except (OSError, subprocess.SubprocessError, re.error):
        return []


def switch_tab(param):
    patterns = blocklist_patterns()
    visible = [
        (index, option)
        for index, option in enumerate(param["opts"])
        if not any(pattern.search(option) for pattern in patterns)
    ]
    selected_options = [option for _, option in visible]
    returncode, selected = rofi_select({**param, "opts": selected_options})
    if returncode != 0 or not selected:
        return ""

    try:
        visible_index = selected_options.index(selected)
    except ValueError:
        return "g " + selected

    original_index = visible[visible_index][0]
    tab_ids = param.get("tabIds", [])
    if original_index < len(tab_ids):
        return tab_ids[original_index]
    return selected.rsplit(" ::: ", 1)[-1]


def list_downloads(param):
    returncode, selected = rofi_select(param)
    if not selected:
        return ""
    if returncode == 0:
        subprocess.Popen(["fcp", selected])
    elif returncode == 10:
        subprocess.Popen(["xdg-open", selected])
    return ""


def copy_download(path):
    subprocess.Popen(["fcp", path])
    return ""


def select_option(param):
    returncode, selected = rofi_select(param)
    return selected if returncode == 0 else ""


def handle_message(message):
    info = message.get("info", "")
    param = message.get("param", {})
    handlers = {
        "switchTab": switch_tab,
        "listDownloads": list_downloads,
        "copyDownload": copy_download,
        "openHistory": select_option,
        "changeToPage": select_option,
    }
    handler = handlers.get(info)
    if handler is None:
        return {"result": "", "info": info, "error": f"unknown command: {info}"}
    try:
        return {"result": handler(param), "info": info}
    except Exception as error:
        return {"result": "", "info": info, "error": str(error)}


def main():
    while True:
        try:
            message = read_message()
            if message is None:
                return
            send_message(handle_message(message))
        except (EOFError, ValueError, json.JSONDecodeError) as error:
            print(error, file=sys.stderr)
            return


if __name__ == "__main__":
    main()
