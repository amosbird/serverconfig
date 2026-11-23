#!/usr/bin/env python3
"""Bootstrap smoke probe for the isolated real-extension/native-chain fixture."""
import json
import os
import pathlib
import subprocess
import tempfile

assert os.environ.get("ABUSH_ENGINE_FIXTURE") == "1"
repo = pathlib.Path(__file__).resolve().parent.parent
common = pathlib.Path(subprocess.check_output(
    ["git", "rev-parse", "--path-format=absolute", "--git-common-dir"],
    cwd=repo, text=True, timeout=5).strip())
temporary = common.parent / ".worktrees" / ".tmp"
temporary.mkdir(parents=True, exist_ok=True)
with tempfile.TemporaryDirectory(prefix="e-", dir=temporary) as name:
    root = pathlib.Path(name)
    (root / "r").mkdir(mode=0o700)
    (root / ".cache").mkdir()
    env = dict(os.environ, HOME=name, TMPDIR=name, XDG_CACHE_HOME=str(root / ".cache"),
               ABUSH_ENGINE_REAL_EXTENSION="1")
    for key in ["DISPLAY", "WAYLAND_DISPLAY", "XDG_RUNTIME_DIR"]:
        env.pop(key, None)
    child = subprocess.Popen([
        "node", str(repo / "scripts/browser_engine_chain_fixture.js"),
        str(repo / "chromium/external-links/extension"),
        str(repo / "chromium/external-links/host.py"),
        str(root / "r"), str(root / "artifacts"),
    ], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE,
       text=True, env=env)
    try:
        stdout, stderr = child.communicate('{"op":"bootstrap"}\n{"op":"quit"}\n', timeout=45)
    except subprocess.TimeoutExpired:
        child.terminate()
        try:
            stdout, stderr = child.communicate(timeout=10)
        except subprocess.TimeoutExpired:
            child.kill()
            stdout, stderr = child.communicate(timeout=5)
        raise AssertionError(f"Owned chain smoke deadline: {stderr}")
    assert child.returncode == 0, stderr
    states = [json.loads(line) for line in stdout.splitlines()]
    assert len(states) == 2, stdout
    assert len(states[0]["tabs"]) == 2
    assert states[0]["engineOpenCount"] == 0
    assert states[0]["errors"] == [], states[0]
    assert states[0]["consent"]["browser_boot_id"] == "chain-boot", states[0]
    print("PASS real MV3 extension + NativeDispatcher + Python bootstrap; isolated tabs/storage; bounded shutdown")
