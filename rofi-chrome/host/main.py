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


def switch_tab(param):
    returncode, selected = rofi_select(param)
    if returncode != 0 or not selected:
        return ""

    try:
        index = param["opts"].index(selected)
    except ValueError:
        return "g " + selected

    tab_ids = param.get("tabIds", [])
    if index < len(tab_ids):
        return tab_ids[index]
    return selected.rsplit(" ::: ", 1)[-1]


def list_downloads(param):
    returncode, selected = rofi_select(param)
    if not selected or returncode not in (0, 10):
        return ""
    try:
        index = param["opts"].index(selected)
        download_id = param["downloadIds"][index]
    except (ValueError, IndexError, KeyError):
        return ""
    return {"action": "copy" if returncode == 0 else "open", "id": download_id}


def select_option(param):
    returncode, selected = rofi_select(param)
    return selected if returncode == 0 else ""


def open_in_browser(param):
    url = param.get("url", "")
    if not re.match(r"^https?://", url):
        return ""
    subprocess.Popen(
        ["/home/amos/scripts/chromium", url],
        stdout=subprocess.DEVNULL,
        stderr=subprocess.DEVNULL,
    )
    return ""


def handle_message(message):
    info = message.get("info", "")
    param = message.get("param", {})
    handlers = {
        "switchTab": switch_tab,
        "listDownloads": list_downloads,
        "openHistory": select_option,
        "changeToPage": select_option,
        "openInBrowser": open_in_browser,
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
