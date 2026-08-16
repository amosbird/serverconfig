#!/usr/bin/env python3

import importlib.util
import pathlib
import sys
from unittest import mock

HOST = pathlib.Path(__file__).resolve().parents[1] / "chromium/external-links/host.py"
spec = importlib.util.spec_from_file_location("browser_router", HOST)
host = importlib.util.module_from_spec(spec)
spec.loader.exec_module(host)

cases = {
    "https://chatgpt.com/": "runai",
    "https://app.slack.com/client/T/C": "runchat",
    "https://example.com/": "chromium",
    "https://accounts.google.com/o/oauth2/auth": None,
}

with mock.patch.object(host, "reuse_tab", return_value=False), mock.patch(
    "subprocess.Popen"
) as popen:
    for url, launcher in cases.items():
        popen.reset_mock()
        result = host.handle_message({"url": url, "sourceUrl": "https://example.org/"})
        if result != {"ok": True}:
            raise SystemExit(f"route failed: {url}: {result}")
        if launcher is None:
            if popen.called:
                raise SystemExit(f"auth URL escaped current browser: {url}")
        elif not popen.called or pathlib.Path(popen.call_args.args[0][0]).name != launcher:
            raise SystemExit(f"wrong launcher for {url}: {popen.call_args}")

print("browser router self-check: OK")
