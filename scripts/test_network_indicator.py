#!/usr/bin/env python3

import importlib.machinery
import importlib.util
import json
import pathlib
import sys
import tempfile
import unittest
from unittest import mock

ROOT = pathlib.Path(__file__).parents[1]
INDICATOR = ROOT / "scripts/network-indicator"
STARTUP = ROOT / "scripts/startup"

sys.path.insert(0, str(ROOT / "scripts"))
import trayorder

loader = importlib.machinery.SourceFileLoader("network_indicator", str(INDICATOR))
spec = importlib.util.spec_from_loader(loader.name, loader)
module = importlib.util.module_from_spec(spec)
loader.exec_module(module)


def sample_networkd(interfaces):
    return {"Interfaces": interfaces}


def sample_iface(name, link_type, operational, ip="", prefix=24, gateway="", metric=600):
    entry = {
        "Name": name,
        "Type": link_type,
        "OperationalState": operational,
        "CarrierState": "carrier" if operational in ("routable", "degraded") else "no-carrier",
        "Addresses": [],
        "Routes": [],
    }
    if ip:
        entry["Addresses"].append(
            {
                "Family": 2,
                "ScopeString": "global",
                "AddressString": ip,
                "PrefixLength": prefix,
            }
        )
    if gateway:
        entry["Routes"].append(
            {
                "Family": 2,
                "DestinationPrefixLength": 0,
                "TableString": "main",
                "GatewayString": gateway,
                "PreferredSourceString": ip,
                "Priority": metric,
            }
        )
    return entry


def sample_iwd(name="wlan0", state="connected", ssid="RCMK"):
    net_path = "/net/connman/iwd/0/4/net"
    dev_path = "/net/connman/iwd/0/4"
    objects = {
        dev_path: {
            "net.connman.iwd.Device": {"Name": name, "Powered": True},
            "net.connman.iwd.Station": {
                "State": state,
                "ConnectedNetwork": net_path if ssid else "",
            },
            "net.connman.iwd.StationDiagnostic": {},
        }
    }
    if ssid:
        objects[net_path] = {"net.connman.iwd.Network": {"Name": ssid}}
    return objects


class NetworkIndicatorTest(unittest.TestCase):
    def test_physical_interface_filter_excludes_virtual_links(self):
        self.assertTrue(module.is_physical_interface("wlan0", "wlan"))
        self.assertTrue(module.is_physical_interface("enp9s0u2u1u2", "ether"))
        self.assertFalse(module.is_physical_interface("lo", "loopback"))
        self.assertFalse(module.is_physical_interface("veth-ioa", "ether"))
        self.assertFalse(module.is_physical_interface("tailscale0", "none"))
        self.assertFalse(module.is_physical_interface("docker0", "bridge"))

    def test_signal_bars_thresholds(self):
        self.assertIsNone(module.signal_bars(None))
        self.assertEqual(module.signal_bars(-45), 4)
        self.assertEqual(module.signal_bars(-55), 4)
        self.assertEqual(module.signal_bars(-60), 3)
        self.assertEqual(module.signal_bars(-70), 2)
        self.assertEqual(module.signal_bars(-80), 1)
        self.assertEqual(module.signal_bars(-92), 0)

    def test_signal_bars_hysteresis_dampens_boundary_oscillation(self):
        # Staying at 3 bars across small dips below -67 dBm, stepping down only
        # once RSSI drops past the hysteresis band (-67 - 3 = -70 dBm).
        self.assertEqual(module.signal_bars(-68, previous=3), 3)
        self.assertEqual(module.signal_bars(-70, previous=3), 3)
        self.assertEqual(module.signal_bars(-71, previous=3), 2)
        # Climbing back from 2 bars requires crossing -67 + 3 = -64 dBm.
        self.assertEqual(module.signal_bars(-67, previous=2), 2)
        self.assertEqual(module.signal_bars(-65, previous=2), 2)
        self.assertEqual(module.signal_bars(-64, previous=2), 3)

    def test_build_state_applies_hysteresis_only_for_same_network(self):
        payload = sample_networkd(
            [sample_iface("wlan0", "wlan", "routable", "10.0.0.5", 24, "10.0.0.1", 600)]
        )
        interfaces, routes = module.parse_networkd_describe(payload)
        stations_a = module.parse_iwd_objects(sample_iwd(ssid="RCMK"))
        stations_b = module.parse_iwd_objects(sample_iwd(ssid="OtherAP"))

        prev = module.build_state(
            interfaces, routes, stations_a, {"wlan0": {"AverageRSSI": -64}}
        )
        self.assertEqual(prev.bars, 3)

        # Small dip on the same SSID holds 3 bars.
        same = module.build_state(
            interfaces, routes, stations_a, {"wlan0": {"AverageRSSI": -69}}, previous=prev
        )
        self.assertEqual(same.bars, 3)
        self.assertEqual(module.appearance_color(same), module.WIFI_GOOD_COLOR)

        # Switching to a different SSID resets hysteresis immediately.
        other = module.build_state(
            interfaces, routes, stations_b, {"wlan0": {"AverageRSSI": -69}}, previous=prev
        )
        self.assertEqual(other.bars, 2)
        self.assertEqual(module.appearance_color(other), module.WIFI_FAIR_COLOR)

    def test_ip_fallback_parser_checks_carrier_flags(self):
        addr_json = json.dumps(
            [
                {
                    "ifname": "enp0s3",
                    "flags": ["NO-CARRIER", "BROADCAST", "MULTICAST", "UP"],
                    "operstate": "DOWN",
                    "addr_info": [{"local": "10.0.0.9", "prefixlen": 24}],
                },
                {
                    "ifname": "wlan0",
                    "flags": ["BROADCAST", "MULTICAST", "UP", "LOWER_UP"],
                    "operstate": "UP",
                    "addr_info": [{"local": "172.20.34.130", "prefixlen": 21}],
                },
            ]
        )
        route_json = json.dumps(
            [{"dst": "default", "gateway": "172.20.32.1", "dev": "wlan0", "metric": 600}]
        )
        interfaces, routes = module.parse_ip_fallback_output(
            addr_json, route_json, is_wireless_fn=lambda name: name == "wlan0"
        )
        self.assertEqual(interfaces["enp0s3"]["operational"], "no-carrier")
        self.assertEqual(interfaces["wlan0"]["operational"], "routable")
        self.assertEqual(interfaces["wlan0"]["type"], "wlan")
        self.assertEqual(routes, [(600, "wlan0", "172.20.32.1", "")])

    def test_load_iwd_holds_last_state_during_brief_outage(self):
        indicator = object.__new__(module.NetworkIndicator)
        indicator.last_iwd = ({}, {}, 0.0)
        stations_raw = sample_iwd(ssid="RCMK")
        diag_raw = {"AverageRSSI": -58, "Frequency": 5745, "Security": "WPA2-Personal"}

        class FakeVariant:
            def __init__(self, value):
                self._value = value

            def unpack(self):
                return (self._value,)

        class FakeBus:
            def __init__(self):
                self.fail_objects = False
                self.fail_diag = False

            def call_sync(self, _dest, _path, _iface, method, *_args):
                if method == "GetManagedObjects":
                    if self.fail_objects:
                        raise module.GLib.Error("iwd restarting")
                    return FakeVariant(stations_raw)
                if method == "GetDiagnostics":
                    if self.fail_diag:
                        raise module.GLib.Error("diag unavailable")
                    return FakeVariant(diag_raw)
                raise AssertionError(method)

        indicator.bus = FakeBus()
        with mock.patch.object(module.time, "monotonic", return_value=100.0):
            stations, diag = indicator.load_iwd()
        self.assertEqual(stations["wlan0"]["ssid"], "RCMK")
        self.assertEqual(diag["wlan0"]["AverageRSSI"], -58)

        # Transient diagnostic glitch keeps previous diagnostics.
        indicator.bus.fail_diag = True
        with mock.patch.object(module.time, "monotonic", return_value=102.0):
            stations, diag = indicator.load_iwd()
        self.assertEqual(diag["wlan0"]["AverageRSSI"], -58)

        # Full iwd restart holds cached state within grace period.
        indicator.bus.fail_objects = True
        with mock.patch.object(module.time, "monotonic", return_value=105.0):
            stations, diag = indicator.load_iwd()
        self.assertEqual(stations["wlan0"]["ssid"], "RCMK")

        # Past grace period, clears cleanly instead of lying forever.
        with mock.patch.object(module.time, "monotonic", return_value=120.0):
            stations, diag = indicator.load_iwd()
        self.assertEqual(stations, {})
        self.assertEqual(diag, {})

    def test_load_holds_connected_state_briefly_when_queries_return_empty(self):
        indicator = object.__new__(module.NetworkIndicator)
        indicator.state = module.State(kind="wifi", interface="wlan0", ssid="RCMK", rssi=-55)
        indicator.last_connected_at = 100.0
        indicator.offline_retry_pending = False
        rechecks = []
        indicator.schedule_offline_recheck = lambda: rechecks.append(True)
        indicator.load_networkd = lambda: ({}, [])
        indicator.load_iwd = lambda: ({}, {})

        with mock.patch.object(module.time, "monotonic", return_value=101.0):
            held = indicator.load()
        self.assertEqual(held.kind, "wifi")
        self.assertEqual(held.ssid, "RCMK")
        self.assertEqual(len(rechecks), 1)

        with mock.patch.object(module.time, "monotonic", return_value=105.0):
            offline = indicator.load()
        self.assertEqual(offline.kind, "offline")

    def test_frequency_and_speed_formatting(self):
        self.assertEqual(module.format_frequency(0), "")
        self.assertEqual(module.format_frequency(2437), "2.4 GHz (2437 MHz)")
        self.assertEqual(module.format_frequency(5745), "5 GHz (5745 MHz)")
        self.assertEqual(module.format_frequency(6115), "6 GHz (6115 MHz)")
        self.assertEqual(module.format_speed(None), "")
        self.assertEqual(module.format_speed(100), "100M")
        self.assertEqual(module.format_speed(1000), "1G")
        self.assertEqual(module.format_speed(2500), "2.5G")

    def test_build_state_prefers_lowest_metric_default_route(self):
        payload = sample_networkd(
            [
                sample_iface("lo", "loopback", "carrier", "127.0.0.1", 8),
                sample_iface("veth-ioa", "ether", "routable", "198.18.170.1", 30),
                sample_iface("tailscale0", "none", "routable", "100.88.203.53", 32),
                sample_iface("wlan0", "wlan", "routable", "172.20.34.130", 21, "172.20.32.1", 600),
                sample_iface("enp9s0", "ether", "routable", "10.0.0.10", 24, "10.0.0.1", 100),
            ]
        )
        interfaces, routes = module.parse_networkd_describe(payload)
        stations = module.parse_iwd_objects(sample_iwd())
        state = module.build_state(
            interfaces, routes, stations, {}, link_speed_fn=lambda _iface: 1000
        )
        self.assertEqual(state.kind, "wired")
        self.assertEqual(state.interface, "enp9s0")
        self.assertEqual(state.ip, "10.0.0.10/24")
        self.assertEqual(state.gateway, "10.0.0.1")
        self.assertEqual(state.speed_mbps, 1000)
        self.assertEqual(state.tailscale_ip, "100.88.203.53")
        self.assertEqual(module.short_label(state), "1G")
        self.assertEqual(module.appearance_color(state), module.WIRED_COLOR)

    def test_build_state_wifi_extracts_ssid_and_diagnostics(self):
        payload = sample_networkd(
            [
                sample_iface("veth-ioa", "ether", "routable", "198.18.170.1", 30),
                sample_iface("tailscale0", "none", "routable", "100.88.203.53", 32),
                sample_iface("wlan0", "wlan", "routable", "172.20.34.130", 21, "172.20.32.1", 600),
            ]
        )
        interfaces, routes = module.parse_networkd_describe(payload)
        stations = module.parse_iwd_objects(sample_iwd(ssid="RCMK"))
        diagnostics = {
            "wlan0": {
                "AverageRSSI": -55,
                "Frequency": 5745,
                "Security": "WPA2-Personal",
            }
        }
        state = module.build_state(interfaces, routes, stations, diagnostics)
        self.assertEqual(state.kind, "wifi")
        self.assertEqual(state.ssid, "RCMK")
        self.assertEqual(state.rssi, -55)
        self.assertEqual(state.frequency, 5745)
        self.assertEqual(state.security, "WPA2-Personal")
        self.assertEqual(module.short_label(state), "RCMK")
        self.assertEqual(module.appearance_color(state), module.WIFI_GOOD_COLOR)
        lines = module.tooltip_lines(state)
        self.assertEqual(lines[0], "Wi-Fi  RCMK")
        self.assertIn("-55 dBm  ·  5 GHz (5745 MHz)  ·  WPA2-Personal", lines)
        self.assertIn("Interface  wlan0", lines)
        self.assertIn("IP  172.20.34.130/21", lines)
        self.assertIn("Gateway  172.20.32.1", lines)
        self.assertIn("Tailscale  100.88.203.53", lines)

    def test_captive_portal_overrides_color_and_label(self):
        state = module.State(
            kind="wifi",
            interface="wlan0",
            ssid="HotelWiFi",
            ip="10.1.2.3/24",
            rssi=-52,
            portal_url="https://portal.example/login",
        )
        self.assertEqual(module.short_label(state), "LOGIN")
        self.assertEqual(module.appearance_color(state), module.PORTAL_COLOR)
        self.assertIn("Captive portal — click to open login", module.tooltip_lines(state))

    def test_connecting_and_offline_states(self):
        interfaces, routes = module.parse_networkd_describe(sample_networkd([]))
        connecting_stations = module.parse_iwd_objects(
            sample_iwd(state="connecting", ssid="Tencent-WiFi")
        )
        connecting = module.build_state(interfaces, routes, connecting_stations, {})
        self.assertEqual(connecting.kind, "connecting")
        self.assertEqual(module.short_label(connecting), "···")
        self.assertEqual(module.appearance_color(connecting), module.CONNECTING_COLOR)
        self.assertEqual(
            module.tooltip_lines(connecting)[:2],
            ["Wi-Fi  Tencent-WiFi", "Connecting…"],
        )

        offline = module.build_state(interfaces, routes, {}, {})
        self.assertEqual(offline.kind, "offline")
        self.assertEqual(module.short_label(offline), "OFF")
        self.assertEqual(module.appearance_color(offline), module.OFFLINE_COLOR)
        self.assertEqual(module.tooltip_lines(offline), ["Network  Disconnected"])

    def test_read_portal_url_handles_present_and_missing_file(self):
        with tempfile.TemporaryDirectory() as tmp:
            missing = pathlib.Path(tmp) / "captive-portal-url"
            self.assertEqual(module.read_portal_url(str(missing)), "")
            missing.write_text("https://portal.example/auth?token=abc\nignored\n")
            self.assertEqual(
                module.read_portal_url(str(missing)),
                "https://portal.example/auth?token=abc",
            )

    def test_rssi_jitter_within_same_bar_keeps_icon_key_stable(self):
        a = module.State(kind="wifi", interface="wlan0", ssid="RCMK", rssi=-50)
        b = module.State(kind="wifi", interface="wlan0", ssid="RCMK", rssi=-54)
        c = module.State(kind="wifi", interface="wlan0", ssid="RCMK", rssi=-70)
        self.assertEqual(a.key(), b.key())
        self.assertNotEqual(a.key(), c.key())

    def test_fit_label_layout_shrinks_and_wraps_to_two_lines_at_min_px(self):
        surface = module.cairo.ImageSurface(module.cairo.FORMAT_ARGB32, 96, 96)
        ctx = module.cairo.Context(surface)

        short_layout, short_px = module.NetworkIndicator.fit_label_layout(ctx, "RCMK")
        mid_layout, mid_px = module.NetworkIndicator.fit_label_layout(ctx, "Office-5G")
        two_line_layout, two_line_px = module.NetworkIndicator.fit_label_layout(
            ctx, "ChinaNet-5G-Office"
        )
        cjk_layout, cjk_px = module.NetworkIndicator.fit_label_layout(ctx, "星巴克无线网络")
        overflow_layout, overflow_px = module.NetworkIndicator.fit_label_layout(
            ctx, "ChinaNet-VeryLongSSIDName-5G-Extra"
        )

        self.assertEqual(module.LABEL_MIN_PX, 13)
        self.assertEqual(short_px, module.LABEL_MAX_PX)
        self.assertEqual(short_layout.get_line_count(), 1)
        self.assertFalse(short_layout.is_ellipsized())

        self.assertLess(mid_px, short_px)
        self.assertGreaterEqual(mid_px, module.LABEL_MIN_PX)
        self.assertEqual(mid_layout.get_line_count(), 1)
        self.assertFalse(mid_layout.is_ellipsized())

        self.assertEqual(two_line_px, module.LABEL_MIN_PX)
        self.assertEqual(two_line_layout.get_line_count(), 2)
        self.assertFalse(two_line_layout.is_ellipsized())

        self.assertEqual(cjk_px, module.LABEL_MIN_PX)
        self.assertEqual(cjk_layout.get_line_count(), 2)
        self.assertFalse(cjk_layout.is_ellipsized())

        self.assertEqual(overflow_px, module.LABEL_MIN_PX)
        self.assertEqual(overflow_layout.get_line_count(), 2)
        self.assertTrue(overflow_layout.is_ellipsized())

    def test_icon_is_hidpi_cjk_and_event_driven(self):
        source = INDICATOR.read_text()
        self.assertIn("ICON_SIZE = 96", source)
        self.assertIn("DESIGN_SIZE = 96", source)
        self.assertIn('self.icon.connect("size-changed", self.on_icon_size_changed)', source)
        self.assertIn("PangoCairo.show_layout", source)
        self.assertIn('FONT = "Noto Sans CJK SC"', source)
        self.assertIn("Gio.DBusCallFlags.NO_AUTO_START", source)
        self.assertIn('"NameOwnerChanged"', source)
        self.assertIn('"PrepareForSleep"', source)
        self.assertIn("monitor_file", source)
        self.assertIn("clipboard.store()", source)
        self.assertIn("faulthandler.enable()", source)

    def test_tray_order_startup_and_restore_wire_the_indicator(self):
        self.assertIn("network-indicator", trayorder.ORDER)
        self.assertEqual(trayorder.SLOTS["network-indicator"], 1)
        self.assertIn(
            'run "network-indicator" systemctl --user start network-indicator.service',
            STARTUP.read_text(),
        )
        unit = (ROOT / "systemd/network-indicator.service").read_text()
        self.assertIn("Restart=always", unit)
        self.assertIn("GDK_SCALE=2", unit)
        self.assertIn(
            'install -Dm644 "$DIR/systemd/network-indicator.service"',
            (ROOT / "restore.sh").read_text(),
        )


if __name__ == "__main__":
    unittest.main()
