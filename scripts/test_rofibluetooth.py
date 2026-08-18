#!/usr/bin/env python3

import importlib.machinery
import importlib.util
import pathlib
import subprocess
import unittest
from unittest import mock

SCRIPT = pathlib.Path(__file__).with_name("rofibluetooth-blocks")
loader = importlib.machinery.SourceFileLoader("rofibluetooth_blocks", str(SCRIPT))
spec = importlib.util.spec_from_loader(loader.name, loader)
module = importlib.util.module_from_spec(spec)
loader.exec_module(module)


class BluetoothBlocksTest(unittest.TestCase):
    def setUp(self):
        module.known_devices.clear()

    def test_read_only_queries_do_not_use_bluetoothctl_timeout(self):
        with mock.patch.object(module.subprocess, "run") as run:
            run.return_value = subprocess.CompletedProcess([], 0, "", "")
            module.bt("devices")

        self.assertNotIn("--timeout", run.call_args.args[0])

    def test_menus_enable_native_filtering_and_focus_first_candidate(self):
        self.assertEqual(module.menu_update("main", []), {
            "input action": "filter",
            "active entry": 0,
            "input": "",
            "prompt": "Bluetooth > ",
            "lines": [],
        })
        self.assertEqual(module.menu_update("scan", [{"text": "←  Back"}, {"text": "Device"}]), {
            "input action": "filter",
            "active entry": 1,
            "input": "",
            "prompt": "Bluetooth scan > ",
            "lines": [{"text": "←  Back"}, {"text": "Device"}],
        })

    def test_device_states_use_ascii_to_avoid_font_fallback_baseline_shift(self):
        states = (
            ({"Connected": "yes"}, "[+]"),
            ({"Paired": "yes"}, "[=]"),
            ({}, "[ ]"),
        )
        for info, prefix in states:
            with self.subTest(info=info):
                line = module.device_line("AA:BB:CC:DD:EE:FF", "Headphones", info)
                self.assertTrue(line["text"].startswith(prefix + "  "))

    def test_status_uses_overlay_without_moving_list(self):
        update = module.menu_update(
            "scan", [{"text": "Device"}], "Connected Headphones", clear_input=False
        )

        self.assertEqual(update["overlay"], "Connected Headphones")
        self.assertNotIn("message", update)

    @mock.patch.object(module, "properties")
    @mock.patch.object(module, "bt")
    def test_main_menu_only_shows_paired_devices_and_scan(self, bt, properties):
        bt.return_value = subprocess.CompletedProcess(
            [], 0, "Device AA:BB:CC:DD:EE:FF Headphones\nDevice 11:22:33:44:55:66 TV\n", ""
        )
        properties.side_effect = [
            {"Name": "Headphones", "Paired": "yes", "Connected": "no"},
            {"Name": "TV", "Paired": "no", "Connected": "no"},
        ]

        self.assertEqual(
            module.main_lines(),
            [
                {"text": "↻  Scan for devices", "data": "scan"},
                {"text": "[=]  Headphones", "data": "device:AA:BB:CC:DD:EE:FF"},
            ],
        )

    @mock.patch.object(module, "properties", return_value={"Name": "Headphones"})
    @mock.patch.object(module, "bt")
    def test_scan_menu_is_a_secondary_menu_with_back(self, bt, properties):
        bt.return_value = subprocess.CompletedProcess(
            [], 0, "Device AA:BB:CC:DD:EE:FF Headphones\n", ""
        )

        self.assertEqual(
            module.scan_lines(),
            [
                {"text": "←  Back", "data": "back"},
                {"text": "[ ]  Headphones", "data": "device:AA:BB:CC:DD:EE:FF"},
            ],
        )

    @mock.patch.object(module, "bt")
    @mock.patch.object(module.subprocess, "Popen")
    def test_scanner_explicitly_turns_scan_off(self, popen, bt):
        process = popen.return_value
        scanner = module.Scanner()
        scanner.start()
        scanner.stop()

        process.stdin.write.assert_has_calls([mock.call("scan on\n"), mock.call("scan off\n")])
        process.terminate.assert_called_once_with()

    @mock.patch.object(module, "bt")
    @mock.patch.object(module.subprocess, "Popen")
    def test_scanner_falls_back_to_one_shot_scan_off(self, popen, bt):
        scanner = module.Scanner()
        scanner.start()
        scanner.stop()

        bt.assert_called_once_with("scan", "off")

    @mock.patch.object(module, "notify")
    @mock.patch.object(module, "pair_and_connect", return_value=(True, "Connected Headphones"))
    @mock.patch.object(module, "send")
    def test_device_action_keeps_menu_alive_while_pairing(self, send, pair, notify):
        worker = module.start_device_action(
            "AA:BB:CC:DD:EE:FF", "Headphones", lambda: [{"text": "←  Back"}], lambda: None
        )
        worker.join()

        self.assertEqual(
            send.call_args_list[0],
            mock.call(
                module.menu_update(
                    "scan", [{"text": "←  Back"}], "Pairing Headphones…", clear_input=False
                )
            ),
        )
        self.assertEqual(
            send.call_args_list[-1],
            mock.call(
                module.menu_update(
                    "scan", [{"text": "←  Back"}], "Connected Headphones", clear_input=False
                )
            ),
        )
        notify.assert_called_once_with("Connected Headphones")

    @mock.patch.object(module, "notify")
    @mock.patch.object(module, "pair_and_connect", return_value=(True, "Connected Headphones"))
    @mock.patch.object(module, "send")
    def test_device_action_stops_scan_only_after_pairing(self, send, pair, notify):
        scanner = mock.Mock()
        worker = module.start_device_action(
            "AA:BB:CC:DD:EE:FF",
            "Headphones",
            lambda: [{"text": "←  Back"}],
            scanner.stop,
        )
        worker.join()

        scanner.stop.assert_called_once_with()
        self.assertEqual(pair.mock_calls[0], mock.call("AA:BB:CC:DD:EE:FF"))
        notify.assert_called_once_with("Connected Headphones")

    @mock.patch.object(module, "wait_for", return_value=True)
    @mock.patch.object(module, "properties")
    def test_pair_keeps_one_bluetoothctl_session_alive_until_trusted(self, properties, wait_for):
        properties.return_value = {"Name": "Headphones", "Paired": "no"}
        session = mock.MagicMock()

        ok, _ = module.pair_and_connect(
            "AA:BB:CC:DD:EE:FF", session_factory=lambda: session
        )

        self.assertTrue(ok)
        self.assertEqual(
            session.command.call_args_list,
            [mock.call("pair AA:BB:CC:DD:EE:FF"), mock.call("trust AA:BB:CC:DD:EE:FF"), mock.call("connect AA:BB:CC:DD:EE:FF")],
        )
        session.close.assert_called_once_with()

    @mock.patch.object(module, "wait_for", return_value=False)
    @mock.patch.object(module, "properties")
    def test_pair_requires_confirmed_paired_state_before_connect(self, properties, wait_for):
        properties.return_value = {"Name": "Headphones", "Paired": "no"}
        session = mock.MagicMock()

        ok, _ = module.pair_and_connect(
            "AA:BB:CC:DD:EE:FF", session_factory=lambda: session
        )

        self.assertFalse(ok)
        self.assertNotIn(mock.call("connect AA:BB:CC:DD:EE:FF"), session.command.call_args_list)


if __name__ == "__main__":
    unittest.main()
