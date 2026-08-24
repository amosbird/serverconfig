#!/usr/bin/env python3

import importlib.machinery
import importlib.util
import pathlib
import unittest

ROOT = pathlib.Path(__file__).parents[1]
SCRIPT = ROOT / "scripts/bluetooth-audio-default"


def load_router():
    loader = importlib.machinery.SourceFileLoader("bluetooth_audio_default", str(SCRIPT))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


class FakeOps:
    def __init__(self):
        self.sinks = [{"index": 10, "name": "bluez_output.headset", "mute": True}]
        self.sources = [{"index": 11, "name": "bluez_input.headset", "mute": True}]
        self.sink_inputs = [{"index": 20, "sink": 5, "mute": True}]
        self.source_outputs = [
            {"index": 21, "source": 6, "mute": True, "properties": {}}
        ]
        self.default_sink = "alsa_output.local"
        self.default_source = "alsa_input.local"
        self.sink_mute = None
        self.source_mute = None
        self.calls = []

    def snapshot(self):
        return self.sinks, self.sources, self.sink_inputs, self.source_outputs

    def defaults(self):
        return self.default_sink, self.default_source

    def desired_mutes(self):
        return self.sink_mute, self.source_mute

    def call(self, *args):
        self.calls.append(args)


class BluetoothAudioDefaultTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.module = load_router()

    def test_connected_headset_is_default_without_inventing_mute_state(self):
        ops = FakeOps()
        router = self.module.Router(ops)
        router.route()
        self.assertIn(("set-default-sink", "bluez_output.headset"), ops.calls)
        self.assertIn(("set-default-source", "bluez_input.headset"), ops.calls)
        self.assertFalse(any("mute" in call[0] for call in ops.calls))

    def test_existing_application_streams_move_without_changing_mute(self):
        ops = FakeOps()
        router = self.module.Router(ops)
        router.route()
        self.assertIn(("move-sink-input", "20", "bluez_output.headset"), ops.calls)
        self.assertIn(("move-source-output", "21", "bluez_input.headset"), ops.calls)
        self.assertFalse(any(call[0] == "set-sink-input-mute" for call in ops.calls))
        self.assertFalse(any(call[0] == "set-source-output-mute" for call in ops.calls))

    def test_recreated_headset_restores_explicit_mute_intent(self):
        ops = FakeOps()
        ops.sink_mute = True
        ops.source_mute = False
        router = self.module.Router(ops)
        router.route()
        self.assertIn(("set-sink-mute", "bluez_output.headset", "1"), ops.calls)
        self.assertIn(("set-source-mute", "bluez_input.headset", "0"), ops.calls)
        ops.sinks[0]["index"] = 30
        ops.sources[0]["index"] = 31
        ops.default_sink = "bluez_output.headset"
        ops.default_source = "bluez_input.headset"
        ops.calls.clear()
        router.route()
        self.assertIn(("set-sink-mute", "bluez_output.headset", "1"), ops.calls)
        self.assertIn(("set-source-mute", "bluez_input.headset", "0"), ops.calls)

    def test_later_explicit_mute_is_preserved(self):
        ops = FakeOps()
        router = self.module.Router(ops)
        router.route()
        ops.default_sink = "bluez_output.headset"
        ops.default_source = "bluez_input.headset"
        ops.calls.clear()
        router.route()
        self.assertNotIn(("set-default-sink", "bluez_output.headset"), ops.calls)
        self.assertNotIn(("set-default-source", "bluez_input.headset"), ops.calls)
        self.assertFalse(any("mute" in call[0] for call in ops.calls))

    def test_watchdog_probe_is_never_moved_or_unmuted(self):
        ops = FakeOps()
        ops.source_outputs[0]["properties"] = {
            "application.name": "bluetooth-sco-watchdog"
        }
        router = self.module.Router(ops)
        router.route()
        self.assertFalse(any(call[0] == "move-source-output" for call in ops.calls))
        self.assertFalse(any(call[0] == "set-source-output-mute" for call in ops.calls))


if __name__ == "__main__":
    unittest.main()
