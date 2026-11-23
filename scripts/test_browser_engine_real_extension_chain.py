#!/usr/bin/env python3
"""Real extension/native/engine diagnostics with modeled v10 ingress (no Rust build)."""
import base64
import json
import os
import pathlib
import queue
import subprocess
import tempfile
import threading
import time

from test_browser_peer_broker import FakePeer

assert os.environ.get("ABUSH_ENGINE_FIXTURE") == "1"
repo = pathlib.Path(__file__).resolve().parent.parent
common = pathlib.Path(subprocess.check_output(
    ["git", "rev-parse", "--path-format=absolute", "--git-common-dir"],
    cwd=repo, text=True, timeout=5).strip())
temporary = common.parent / ".worktrees" / ".tmp"
temporary.mkdir(parents=True, exist_ok=True)


class DiagnosticPeer(FakePeer):
    def run(self):
        while not self.stopping.is_set():
            super().run()

    def wait_for(self, kind, request_id=None):
        deadline = time.monotonic() + 20
        with self.condition:
            while True:
                found = next((message for message in self.received if message["type"] == kind and
                              (request_id is None or message.get("request_id") == request_id)), None)
                if found:
                    return found
                remaining = deadline - time.monotonic()
                if remaining <= 0:
                    raise AssertionError(f"missing {kind}/{request_id}: {self.received[-4:]}")
                self.condition.wait(remaining)


with tempfile.TemporaryDirectory(prefix="e-", dir=temporary) as name:
    root = pathlib.Path(name)
    connection = root / "r" / "r_12345678" / "c_12345678"
    connection.mkdir(parents=True, mode=0o700)
    for directory in [root / "r", connection.parent]:
        directory.chmod(0o700)
    socket = connection / "browser-peer.sock"
    metadata = connection / "connection.json"
    metadata.write_text(json.dumps({"schema_version": 1, "remote_key": "r_12345678",
        "connection_id": "c_12345678", "display_name": "chain", "browser_socket": str(socket),
        "remote_browser_socket": "/offline/abush/browser-peer.sock"}))
    metadata.chmod(0o600)
    peer = DiagnosticPeer(socket)
    (root / ".cache").mkdir()
    env = dict(os.environ, HOME=name, TMPDIR=name, XDG_CACHE_HOME=str(root / ".cache"),
               ABUSH_ENGINE_REAL_EXTENSION="1")
    for key in ["DISPLAY", "WAYLAND_DISPLAY", "XDG_RUNTIME_DIR"]:
        env.pop(key, None)
    child = subprocess.Popen([
        "node", str(repo / "scripts/browser_engine_chain_fixture.js"),
        str(repo / "chromium/external-links/extension"),
        str(repo / "chromium/external-links/host.py"), str(root / "r"), str(root / "artifacts"),
    ], stdin=subprocess.PIPE, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True, env=env)
    responses = queue.Queue()
    diagnostics = []

    def read_output():
        try:
            for line in child.stdout:
                responses.put(json.loads(line))
        finally:
            responses.put(None)

    def read_errors():
        for line in child.stderr:
            diagnostics.append(line)

    readers = [threading.Thread(target=read_output, daemon=True),
               threading.Thread(target=read_errors, daemon=True)]
    for reader in readers:
        reader.start()

    def control(op):
        child.stdin.write(json.dumps({"op": op}) + "\n")
        child.stdin.flush()
        try:
            result = responses.get(timeout=25)
        except queue.Empty:
            raise AssertionError(f"fixture {op} deadline: {''.join(diagnostics)}")
        assert result is not None, "".join(diagnostics)
        return result

    sequence = 0

    def execute(command, expect=True):
        global sequence
        sequence += 1
        request = f"diagnostic-{sequence}"
        peer.send({"type": "execute", "request_id": request, "effect_id": request,
            "executor_fence": peer.fence, "authorization": authorization, "command": command})
        try:
            completion = peer.wait_for("completion", request)
        except AssertionError as error:
            raise AssertionError(f"{error}\n{''.join(diagnostics)}")
        if expect:
            assert completion["completion"] == "applied", (completion, "".join(diagnostics))
        return completion

    def action(kind, **fields):
        return execute({"command": "action", "page": page, "action": {"type": kind, **fields}})["result"]

    try:
        enabled = control("ready")
        assert enabled["engineOpenCount"] == 0
        assert len(enabled["tabs"]) == 2
        rejected = control("diagnostics_rejections")
        assert rejected["rejected"] == [None, enabled["tabs"][0]["id"]]
        assert rejected["scriptingCalls"] == 0
        page = enabled["state"]["binding"]["page"]
        authorization = {"browser_boot_id": enabled["state"]["browser_boot_id"],
            "enable_id": enabled["state"]["binding"]["enable_id"],
            "revision": enabled["state"]["revision"]}
        navigate = execute({"command": "navigate", "page": page, "url": "http://fixture.test/"})
        materialized = control("snapshot")
        assert materialized["engineOpenCount"] == 1, materialized
        assert len(materialized["tabs"]) == 3
        assert control("diagnostics_rejections") == rejected
        assert materialized["errors"] == [], (materialized["errors"], "".join(diagnostics))
        print("PASS real extension -> WorkspaceExecutor -> native Python -> inherited AF_UNIX Node -> chrome.debugger navigation")
        snapshot = action("snapshot")
        refs = {name: next(line for line in snapshot["text"].splitlines() if f'"{name}"' in line)
                .split("[ref=")[1].split("]")[0] for name in ["Name", "Load", "Receipt"]}
        action("fill", snapshot_id=snapshot["data"]["snapshot_id"], ref=refs["Name"], value="Ada")
        action("click", snapshot_id=snapshot["data"]["snapshot_id"], ref=refs["Load"])
        control("api_finished")
        action("wait", text="Ready 42", timeout_ms=5000)
        network = action("network_list")
        request = next(request for request in network["data"]["requests"]
                       if request["url"] == "http://fixture.test/api")
        body = action("network_body", request_id=request["request_id"])
        assert body["data"]["json"] == {"answer": 42}
        shot = action("screenshot", full_page=False)
        png = base64.b64decode(shot["images"][0]["data_base64"])
        assert png[:8] == b"\x89PNG\r\n\x1a\n"
        assert int.from_bytes(png[16:20], "big") == 800
        assert int.from_bytes(png[20:24], "big") == 600
        print(f"PASS refs/fill/click/wait/network/body/PNG {int.from_bytes(png[16:20], 'big')}x{int.from_bytes(png[20:24], 'big')}")
        control("lose_receipt")
        action("click", snapshot_id=snapshot["data"]["snapshot_id"], ref=refs["Receipt"])
        reconnected = control("receipt_reconnected")
        assert reconnected["state"]["phase"] == "enabled"
        assert reconnected["state"]["binding"]["page"] == page
        authorization["revision"] = reconnected["state"]["revision"]
        reads = [call for call in reconnected["calls"] if call["name"] == "scripting.executeScript"]
        assert reads[-1]["debuggerAttached"] is False, reads
        assert reads[-1]["phase"] == "completed" and reads[-1]["documentId"], reads
        assert reconnected["engineOpenCount"] == 1, reconnected
        print("PASS receipt-loss reconnect: exact inactive DOM read without debugger reattachment")
        control("snapshot")
        restarted = control("restart_worker")
        authorization["revision"] = restarted["state"]["revision"]
        assert "Receipt once" in action("text")["text"]
        control("arm_wait")
        sequence += 1
        waiting = f"diagnostic-{sequence}"
        peer.send({"type": "execute", "request_id": waiting, "effect_id": waiting,
            "executor_fence": peer.fence, "authorization": authorization,
            "command": {"command": "action", "page": page,
                "action": {"type": "wait", "text": "Never appears", "timeout_ms": 30000}}})
        control("wait_entered")
        stopped = control("stop")
        assert stopped["state"]["phase"] != "enabled"
        final = control("snapshot")
        assert final["posts"] == 1
        assert final["engineExecuteCount"] == 11, final
        assert final["errors"] == ["Engine lease revoked", "Engine lease revoked"], final["errors"]
        assert final["engineOpenCount"] == 2
        before_read = len(final["calls"])
        detached = control("page_diagnostics")
        assert detached["inputValue"] == "Ada" and detached["receiptClicks"] == 1, detached
        observed = control("snapshot")
        assert observed["calls"][before_read:] == [{
            "name": "scripting.executeScript", "method": None,
            "source": {"tabId": final["consent"]["records"][0]["tab_id"]},
            "phase": "completed", "debuggerAttached": False,
            "documentId": reads[-1]["documentId"],
        }], observed["calls"][before_read:]
        assert observed["engineOpenCount"] == final["engineOpenCount"]
        assert observed["engineExecuteCount"] == final["engineExecuteCount"]
        assert final["wal"]["effects"][f"r_12345678\0principal\0{waiting}"]["completion"] == "unknown"
        print("Observed modeled-ingress Stop: unknown mutation + two surfaced Engine lease revoked advisories (no daemon cancel)")
        assert all(not call["name"].startswith("windows.") for call in final["calls"])
        assert all(not any(name in (call["method"] or "") for name in
                   ["Browser.", "bringToFront", "activateTarget", "closeTarget", "setDeviceMetrics", "setWindow"])
                   for call in final["calls"])
        assert all(call["source"]["tabId"] == final["consent"]["records"][0]["tab_id"]
                   for call in final["calls"] if call["name"] == "debugger.sendCommand")
        print(f"PASS worker reconstruction, Stop bounded waiting exit; {len(final['frames'])} state frames; 11 calls; personal tabs unchanged")
        started = control("start")
        assert started["state"]["phase"] == "enabled"
        assert started["state"]["binding"]["page"] == page
        assert len(started["tabs"]) == 3
        authorization["revision"] = started["state"]["revision"]
        assert "Receipt once" in action("text")["text"]
        print("PASS global Start revalidates same exact work tab and real engine read")
        control("snapshot")
        control("quit")
        assert child.wait(timeout=15) == 0, "".join(diagnostics)
    finally:
        peer.close()
        if child.poll() is None:
            child.stdin.close()
            try:
                child.wait(timeout=15)
            except subprocess.TimeoutExpired:
                child.terminate()
                try:
                    child.wait(timeout=5)
                except subprocess.TimeoutExpired:
                    child.kill()
                    child.wait(timeout=5)
        for reader in readers:
            reader.join(timeout=2)
            assert not reader.is_alive(), "owned fixture reader teardown"
