#!/usr/bin/env python3

import importlib.machinery
import importlib.util
import pathlib
import unittest


SCRIPT = pathlib.Path(__file__).with_name("ioa-open")
loader = importlib.machinery.SourceFileLoader("ioa_open", str(SCRIPT))
spec = importlib.util.spec_from_loader(loader.name, loader)
ioa = importlib.util.module_from_spec(spec)
loader.exec_module(ioa)

# The URI Chrome actually handed to the portal, with iOA's unencoded page inside.
REAL = (
    "ioa://message/?id=10101&uri=https://devc.woa.com/open/openEnv"
    "?envId=evnIns-7bjmnjtqxlde&bs=&ide=VSCode&version=&platformid="
    "&openAlarmDialog=true"
)


class IoaOpenTest(unittest.TestCase):
    def test_unencoded_page_keeps_its_own_query_string(self):
        self.assertEqual(
            ioa.embedded_url(REAL),
            "https://devc.woa.com/open/openEnv?envId=evnIns-7bjmnjtqxlde&bs="
            "&ide=VSCode&version=&platformid=&openAlarmDialog=true",
        )

    def test_encoded_page_ends_at_the_next_outer_parameter(self):
        self.assertEqual(
            ioa.embedded_url(
                "ioa://open?url=https%3A%2F%2Fexample.internal%2Fa%2Fb&token=abc"
            ),
            "https://example.internal/a/b",
        )

    def test_ide_launch_is_recognised_with_its_parameters(self):
        self.assertEqual(
            ioa.open_env_request(ioa.embedded_url(REAL)),
            ("evnIns-7bjmnjtqxlde", "VSCode", ""),
        )

    def test_openEnv_actions_without_an_ide_are_not_reopened(self):
        # These would emit the same ioa:// URI again and loop.
        url = "https://devc.woa.com/open/openEnv?action=ssh&envId=evnIns-7bjmnjtqxlde"
        self.assertTrue(ioa.OPEN_ENV.match(url))
        self.assertIsNone(ioa.open_env_request(url))

    def test_other_devc_pages_are_opened_normally(self):
        self.assertIsNone(ioa.OPEN_ENV.match("https://devc.woa.com/home"))

    def test_ssh_target_of_a_remote_deep_link(self):
        self.assertEqual(
            ioa.ssh_target(
                "vscode://vscode-remote/ssh-remote+root@tianqizheng-any9"
                ".devcloud.woa.com:36000/data/workspace/?windowId=_blank"
            ),
            ("root", "tianqizheng-any9.devcloud.woa.com", "36000"),
        )

    def test_links_without_an_ssh_target_are_left_alone(self):
        # JetBrains Gateway and the browser terminals need no key check here.
        self.assertIsNone(ioa.ssh_target("https://abetl.devcloud.woa.com:6901"))

    def test_handler_presence_is_reported_per_scheme(self):
        self.assertTrue(ioa.has_handler("vscode://vscode-remote/ssh-remote+root@h/"))
        self.assertFalse(ioa.has_handler("codebuddycn://vscode-remote/x"))

    def test_install_command_carries_the_public_key(self):
        command = ioa.install_key_command()
        with open(ioa.PUBKEY) as handle:
            self.assertIn(handle.read().strip(), command)
        self.assertIn("authorized_keys", command)

    def test_uri_without_a_page_is_refused(self):
        self.assertIsNone(ioa.embedded_url("ioa://message/?id=10101"))

    def test_nested_ioa_scheme_cannot_reenter_the_handler(self):
        self.assertIsNone(ioa.embedded_url("ioa://message/?uri=ioa://message/?id=1"))

    def test_scheme_is_registered_and_defaulted_to_this_handler(self):
        repo = SCRIPT.parent.parent
        desktop = (
            repo / ".local" / "share" / "applications" / "ioa-open.desktop"
        ).read_text()
        mimeapps = (repo / ".config" / "mimeapps.list").read_text()

        self.assertIn("MimeType=x-scheme-handler/ioa;", desktop)
        self.assertIn("Exec=/home/amos/scripts/ioa-open %u", desktop)
        self.assertEqual(
            mimeapps.count("x-scheme-handler/ioa=ioa-open.desktop;"), 2
        )


if __name__ == "__main__":
    unittest.main()
