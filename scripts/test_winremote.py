#!/usr/bin/env python3

import importlib.machinery
import importlib.util
import os
import pathlib
import subprocess
import tempfile
import unittest


ROOT = pathlib.Path(__file__).parents[1]
SCRIPT = ROOT / "scripts/winremote"


def load_winremote():
    loader = importlib.machinery.SourceFileLoader("winremote_mod", str(SCRIPT))
    spec = importlib.util.spec_from_loader(loader.name, loader)
    assert spec is not None
    module = importlib.util.module_from_spec(spec)
    loader.exec_module(module)
    return module


MOD = load_winremote()


class WinremoteUnitTest(unittest.TestCase):
    def test_is_x224_cc(self):
        valid = bytes.fromhex("030000130ed00000123400022f080002000000")
        self.assertTrue(MOD.is_x224_cc(valid))
        self.assertFalse(MOD.is_x224_cc(b""))
        self.assertFalse(MOD.is_x224_cc(b"\x03\x00\x00\x0b\x06\xe0\x00\x00\x00\x00\x00"))
        self.assertFalse(MOD.is_x224_cc(b"HTTP/1.1 200 OK"))

    def test_probe_scoring_penalizes_loss_and_jitter(self):
        r_fast_clean = MOD.Route("amos-sg", "10.0.0.1")
        r_lossy = MOD.Route("clickstock", "10.0.0.2")
        r_jittery = MOD.Route("amoshk", "10.0.0.3")

        samples_by_host = {
            "10.0.0.1": [110.0, 114.0, 112.0],
            "10.0.0.2": [95.0, None, 95.0],
            "10.0.0.3": [100.0, 260.0, 110.0],
        }
        cursors = {h: iter(vals) for h, vals in samples_by_host.items()}

        def fake_probe(host, _port, _timeout):
            return next(cursors[host])

        results = MOD.probe_routes(
            [r_fast_clean, r_lossy, r_jittery], probes=3, probe_fn=fake_probe
        )
        best = MOD.choose_best_route(results)
        self.assertIsNotNone(best)
        assert best is not None
        self.assertEqual(best.route.name, "amos-sg")

        by_name = {res.route.name: res for res in results}
        self.assertEqual(by_name["clickstock"].ok, 2)
        self.assertGreater(by_name["clickstock"].score, 390.0)
        self.assertAlmostEqual(by_name["amoshk"].jitter_ms, 160.0)

    def test_resolve_named_route(self):
        self.assertEqual(MOD.resolve_named_route("amos-sg").host, "100.125.242.70")
        self.assertEqual(MOD.resolve_named_route("100.104.31.7").name, "amoshk")
        manual = MOD.resolve_named_route("192.168.1.10:3390")
        self.assertEqual(manual.host, "192.168.1.10")
        self.assertEqual(manual.port, 3390)

    def test_build_rdp_command_defaults_and_options(self):
        cmd = MOD.build_rdp_command(
            host="100.125.242.70",
            port=3389,
            user="Tencent_Go",
            password="secret",
            gfx="avc444",
        )
        self.assertEqual(cmd[0], "xfreerdp")
        self.assertEqual(cmd.count(f"/rdp2tcp:{MOD.RDP2TCP_BIN}"), 1)
        self.assertIn("/gfx:avc444", cmd)
        self.assertIn("/w:3840", cmd)
        self.assertIn("/h:2160", cmd)
        self.assertIn("/v:100.125.242.70", cmd)
        self.assertIn("/p:secret", cmd)
        self.assertIn("/p:***", MOD.redact_command(cmd))
        self.assertNotIn("/p:secret", MOD.redact_command(cmd))

        cmd_custom = MOD.build_rdp_command(
            host="10.0.0.9",
            port=3390,
            user="alice",
            password="pw",
            gfx="rfx",
            sound=True,
            rdp2tcp=False,
            extra_args=["-wallpaper"],
        )
        self.assertFalse(any(arg.startswith("/rdp2tcp:") for arg in cmd_custom))
        self.assertIn("/gfx:rfx", cmd_custom)
        self.assertIn("/sound:sys:pulse", cmd_custom)
        self.assertIn("-wallpaper", cmd_custom)
        self.assertIn("/v:10.0.0.9:3390", cmd_custom)

    def test_cli_dry_run_and_launch(self):
        with tempfile.TemporaryDirectory() as tmp:
            bin_dir = pathlib.Path(tmp)
            rdp_log = bin_dir / "freerdp.log"
            invocations = bin_dir / "invocations.log"

            (bin_dir / "pass").write_text("#!/bin/sh\nprintf 'mock-pass\\n'\n")
            (bin_dir / "pass").chmod(0o755)
            (bin_dir / "xfreerdp").write_text(
                "#!/bin/sh\n"
                f'printf \'xfreerdp NO_PROXY=%s %s\\n\' "$NO_PROXY" "$*" >> {invocations}\n'
            )
            (bin_dir / "xfreerdp").chmod(0o755)
            (bin_dir / "win-sync-theme").write_text(
                f"#!/bin/sh\nprintf 'win-sync-theme\\n' >> {invocations}\n"
            )
            (bin_dir / "win-sync-theme").chmod(0o755)

            env = os.environ | {"PATH": f"{bin_dir}:/usr/bin:/bin"}

            dry = subprocess.run(
                [str(SCRIPT), "--host", "amos-sg", "--gfx", "rfx", "--dry-run", "--", "-wallpaper"],
                check=True,
                capture_output=True,
                text=True,
                env=env,
            )
            self.assertIn("/gfx:rfx", dry.stdout)
            self.assertIn("/v:100.125.242.70", dry.stdout)
            self.assertIn("/p:***", dry.stdout)
            self.assertIn("-wallpaper", dry.stdout)
            self.assertFalse(invocations.exists())

            subprocess.run(
                [str(SCRIPT), "--host", "amoshk", "--log", str(rdp_log)],
                check=True,
                capture_output=True,
                text=True,
                env=env,
            )
            lines = invocations.read_text().splitlines()
            self.assertTrue(any(line.startswith("xfreerdp NO_PROXY=1 ") for line in lines))
            self.assertTrue(any("/v:100.104.31.7" in line for line in lines))
            self.assertTrue(any("/p:mock-pass" in line for line in lines))
            self.assertTrue(any("/gfx:avc444" in line for line in lines))
            self.assertTrue(rdp_log.exists())


if __name__ == "__main__":
    unittest.main()
