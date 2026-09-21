#!/usr/bin/env python3

import importlib.machinery
import importlib.util
import pathlib
import unittest


SCRIPT = pathlib.Path(__file__).with_name("portal-app-chooser")
loader = importlib.machinery.SourceFileLoader("portal_app_chooser", str(SCRIPT))
spec = importlib.util.spec_from_loader(loader.name, loader)
chooser = importlib.util.module_from_spec(spec)
loader.exec_module(chooser)


class FakePending:
    def __init__(self):
        self.path = "/request"
        self.done = False
        self.dialog = None
        self.request = None
        self.delegated = False
        self.choices = []
        self.replies = []

    def reply(self, response, results):
        self.replies.append((int(response), dict(results)))


class PortalAppChooserTest(unittest.TestCase):
    def test_uri_scheme(self):
        self.assertEqual(chooser.uri_scheme("ioa://message/?id=1"), "ioa")
        self.assertEqual(chooser.uri_scheme("Web+IDE:open"), "web+ide")
        self.assertEqual(chooser.uri_scheme("/tmp/file"), "unknown")

    def test_finish_replies_once_and_releases_request(self):
        backend = object.__new__(chooser.AppChooser)
        pending = FakePending()
        backend.pending = {pending.path: pending}
        released = []
        backend.release = released.append

        backend.finish(pending.path, 1, {"choice": ""})
        backend.finish(pending.path, 0, {"choice": "ignored"})

        self.assertEqual(pending.replies, [(1, {"choice": ""})])
        self.assertEqual(released, [pending])

    def test_update_choices_switches_empty_dialog_to_delegate(self):
        backend = object.__new__(chooser.AppChooser)
        pending = FakePending()
        pending.dialog = type("Dialog", (), {"destroy": lambda self: None})()
        backend.pending = {pending.path: pending}
        delegated = []
        backend.delegate = delegated.append

        backend.UpdateChoices(pending.path, ["chromium-amos"])

        self.assertIsNone(pending.dialog)
        self.assertEqual(pending.choices, ["chromium-amos"])
        self.assertEqual(delegated, [pending])


if __name__ == "__main__":
    unittest.main()
