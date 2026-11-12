#!/usr/bin/env python3

import importlib.util
import pathlib
import unittest
from unittest import mock

HOST = pathlib.Path(__file__).resolve().parents[1] / "chromium/external-links/host.py"
spec = importlib.util.spec_from_file_location("browser_router_e2e_host", HOST)
host = importlib.util.module_from_spec(spec)
spec.loader.exec_module(host)


class BrowserRouterContractTest(unittest.TestCase):
    def test_routes_apps_and_submits_ordinary_open_without_popen(self):
        cases = {
            "https://chatgpt.com/": "runai",
            "https://app.slack.com/client/T/C": "runchat",
        }
        with (
            mock.patch.object(host, "reuse_tab", return_value=False),
            mock.patch.object(host.subprocess, "Popen") as popen,
        ):
            for url, launcher in cases.items():
                with self.subTest(url=url):
                    self.assertEqual(
                        host.handle_message({"url": url, "sourceUrl": "https://example.org/"}),
                        {"ok": True},
                    )
                    self.assertEqual(
                        pathlib.Path(popen.call_args.args[0][0]).name,
                        launcher,
                    )

        ordinary = "https://example.com/"
        with (
            mock.patch.object(host, "reuse_tab", return_value=False),
            mock.patch.object(host, "submit_ordinary_open", return_value="applied") as submit,
            mock.patch.object(host.subprocess, "Popen") as popen,
        ):
            self.assertEqual(
                host.handle_message({"url": ordinary, "sourceUrl": "https://example.org/"}),
                {"ok": True},
            )
        submit.assert_called_once_with(ordinary)
        popen.assert_not_called()

    def test_authentication_url_stays_in_calling_browser(self):
        url = "https://accounts.google.com/o/oauth2/auth"
        with (
            mock.patch.object(host, "reuse_tab", return_value=False),
            mock.patch.object(host, "submit_ordinary_open") as submit,
            mock.patch.object(host.subprocess, "Popen") as popen,
        ):
            self.assertEqual(
                host.handle_message({"url": url, "sourceUrl": "https://example.org/"}),
                {"ok": True},
            )
        submit.assert_not_called()
        popen.assert_not_called()


if __name__ == "__main__":
    unittest.main()
