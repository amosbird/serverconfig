#!/usr/bin/env python3

import importlib.machinery
import importlib.util
import pathlib
import unittest

ROOT = pathlib.Path(__file__).parents[1]
SCRIPT = ROOT / "scripts/bluetooth-sco-watchdog"


def load_watchdog():
    loader = importlib.machinery.SourceFileLoader("bluetooth_sco_watchdog", str(SCRIPT))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


class FakeOps:
    def __init__(self, probes=(True, True)):
        self.probes = iter(probes)
        self.muted = False
        self.capture = True
        self.profile = True
        self.connected = True
        self.connect_results = [True]
        self.disconnects = 0
        self.connects = 0
        self.sleeps = []
        self.now = 100.0

    def capture_active(self):
        return self.capture

    def source_muted(self):
        return self.muted

    def hfp_active(self):
        return self.profile

    def device_connected(self):
        return self.connected

    def probe_is_zero(self):
        return next(self.probes)

    def disconnect(self):
        self.disconnects += 1

    def connect(self):
        self.connects += 1
        return self.connect_results.pop(0)

    def sleep(self, seconds):
        self.sleeps.append(seconds)

    def monotonic(self):
        return self.now


class BluetoothScoWatchdogTest(unittest.TestCase):
    @classmethod
    def setUpClass(cls):
        cls.module = load_watchdog()

    def test_skips_without_real_microphone_capture(self):
        ops = FakeOps()
        ops.capture = False
        self.module.Watchdog(ops).check_once()
        self.assertEqual(ops.disconnects, 0)

    def test_skips_explicitly_muted_source(self):
        ops = FakeOps()
        ops.muted = True
        self.module.Watchdog(ops).check_once()
        self.assertEqual(ops.disconnects, 0)

    def test_one_zero_probe_does_not_reconnect(self):
        ops = FakeOps((True, False))
        self.module.Watchdog(ops).check_once()
        self.assertEqual(ops.disconnects, 0)

    def test_two_zero_probes_reconnect_once_with_backoff(self):
        ops = FakeOps()
        ops.connect_results = [False, False, True]
        self.module.Watchdog(ops).check_once()
        self.assertEqual(ops.disconnects, 1)
        self.assertEqual(ops.connects, 3)
        self.assertEqual(ops.sleeps, [2, 0.5, 1.5, 3.5])

    def test_cooldown_prevents_repeated_recovery(self):
        ops = FakeOps((True, True, True, True))
        watchdog = self.module.Watchdog(ops)
        watchdog.check_once()
        watchdog.check_once()
        self.assertEqual(ops.disconnects, 1)

    def test_only_bit_exact_zero_is_stuck(self):
        self.assertTrue(self.module.samples_are_zero(b"\0\0\0\0"))
        self.assertFalse(self.module.samples_are_zero(b"\1\0\0\0"))

    def test_any_active_application_on_freeclip_is_a_real_capture(self):
        outputs = [
            {
                "source": 42,
                "properties": {"application.name": "Google Chrome"},
                "corked": False,
            },
            {
                "source": 42,
                "properties": {"application.name": "Microsoft Teams"},
                "corked": False,
            },
        ]
        self.assertTrue(self.module.has_active_capture(outputs, 42))

    def test_probe_and_other_sources_are_not_real_capture(self):
        outputs = [
            {
                "source": 42,
                "properties": {"application.name": "bluetooth-sco-watchdog"},
                "corked": False,
            },
            {
                "source": 7,
                "properties": {"application.name": "Google Chrome"},
                "corked": False,
            },
        ]
        self.assertFalse(self.module.has_active_capture(outputs, 42))

    def test_corked_conference_capture_is_inactive(self):
        outputs = [
            {
                "source": 42,
                "properties": {"application.name": "Firefox"},
                "corked": True,
            }
        ]
        self.assertFalse(self.module.has_active_capture(outputs, 42))


if __name__ == "__main__":
    unittest.main()
