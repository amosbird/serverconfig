#!/usr/bin/env python3
"""Real native host; only creation-time browser identity is test supplied."""
import importlib.util
import os
import pathlib
import sys
import threading

assert os.environ.get("ABUSH_ENGINE_FIXTURE") == "1"
path = pathlib.Path(sys.argv[1]).resolve()
sys.path.insert(0, str(path.parent))
spec = importlib.util.spec_from_file_location("chain_host", path)
host = importlib.util.module_from_spec(spec)
spec.loader.exec_module(host)
assert host.PEER_PROTOCOL_VERSION == 10
# Fault injection consumes a genuine Rust receipt after durable admission. Neither
# completion nor replay is fabricated: the production peer reconnects and retries it.
receipt_gate = pathlib.Path(os.environ["HOME"]) / "drop-receipt"
original_receipt = host.RemotePeer._handle_completion_accepted

def drop_one_receipt(peer, message):
    if message["type"] == "completion_accepted" and receipt_gate.exists():
        receipt_gate.unlink()
        (receipt_gate.parent / "receipt-lost").write_text(message["effect_id"])
        peer.socket.shutdown(host.socket.SHUT_RDWR)
        return
    return original_receipt(peer, message)

host.RemotePeer._handle_completion_accepted = drop_one_receipt
endpoint = host.NativeBrokerHost(
    input_stream=sys.stdin.buffer, output_stream=sys.stdout.buffer,
    remote_root=pathlib.Path(sys.argv[2]), ordinary_open_factory=None,
    primary_profile=pathlib.Path(os.environ["HOME"]) / "fake-profile",
    browser={"pid": os.getpid(), "starttime": "1", "browser_boot_id": "chain-boot",
             "profile": pathlib.Path(os.environ["HOME"]) / "fake-profile"},
)
deadline = threading.Timer(110, lambda: os._exit(124))
deadline.start()
try:
    endpoint.run()
    assert endpoint.writer_error is None, endpoint.writer_error
    assert not endpoint.writer.is_alive(), "native writer teardown"
    assert not endpoint.refresh_thread.is_alive(), "native refresh teardown"
    assert not any(worker.is_alive() for worker in endpoint.request_workers)
finally:
    deadline.cancel()
