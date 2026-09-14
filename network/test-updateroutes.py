#!/usr/bin/env python3
"""Regression checks for the optional CN prefix-list updater."""

import importlib.machinery
import importlib.util
import os
import pathlib
import tempfile
import unittest
from unittest import mock

SCRIPT = pathlib.Path(__file__).parents[1] / "scripts" / "updateroutes"
LOADER = importlib.machinery.SourceFileLoader("updateroutes", str(SCRIPT))
SPEC = importlib.util.spec_from_loader(LOADER.name, LOADER)
MODULE = importlib.util.module_from_spec(SPEC)
LOADER.exec_module(MODULE)


class UpdateRoutesTest(unittest.TestCase):
    def test_parser_accepts_only_exact_cn_ipv4_allocations(self):
        data = "\n".join(
            [
                "apnic|CN|ipv4|1.0.1.0|256|20260914|allocated",
                "apnic|cn|ipv4|1.0.2.0|512|20260914|assigned",
                "apnic|CN|ipv4|1.0.8.0|256|20260914|available",
                "apnic|JP|ipv4|1.0.16.0|256|20260914|allocated",
                "apnic|CN|ipv6|2400::|32|20260914|allocated",
            ]
        )
        self.assertEqual(
            MODULE.parse_apnic(data, minimum_prefixes=2),
            ["1.0.1.0/24", "1.0.2.0/23"],
        )

    def test_parser_rejects_truncated_source(self):
        data = "apnic|CN|ipv4|1.0.1.0|256|20260914|allocated\n"
        with self.assertRaisesRegex(ValueError, "implausible APNIC result"):
            MODULE.parse_apnic(data, minimum_prefixes=2)

    def test_parser_rejects_invalid_count_and_alignment(self):
        for record in (
            "apnic|CN|ipv4|1.0.1.0|255|20260914|allocated\n",
            "apnic|CN|ipv4|1.0.1.1|256|20260914|allocated\n",
        ):
            with self.subTest(record=record):
                with self.assertRaisesRegex(ValueError, "invalid APNIC IPv4 record"):
                    MODULE.parse_apnic(record, minimum_prefixes=1)

    def test_identical_content_is_a_true_noop(self):
        with tempfile.TemporaryDirectory() as directory:
            output = pathlib.Path(directory) / ".routefile"
            output.write_bytes(b"1.0.1.0/24\n")
            inode = output.stat().st_ino
            mtime = output.stat().st_mtime_ns
            self.assertFalse(MODULE.replace_if_changed(output, b"1.0.1.0/24\n"))
            self.assertEqual(output.stat().st_ino, inode)
            self.assertEqual(output.stat().st_mtime_ns, mtime)

    def test_replacement_is_atomic_and_cleans_temporary_file(self):
        with tempfile.TemporaryDirectory() as directory:
            output = pathlib.Path(directory) / ".routefile"
            output.write_bytes(b"old\n")
            self.assertTrue(MODULE.replace_if_changed(output, b"new\n"))
            self.assertEqual(output.read_bytes(), b"new\n")
            self.assertEqual(list(output.parent.glob(".routefile.*")), [])

    def test_failed_replace_preserves_previous_file(self):
        with tempfile.TemporaryDirectory() as directory:
            output = pathlib.Path(directory) / ".routefile"
            output.write_bytes(b"known-good\n")
            with mock.patch.object(os, "replace", side_effect=OSError("injected")):
                with self.assertRaisesRegex(OSError, "injected"):
                    MODULE.replace_if_changed(output, b"candidate\n")
            self.assertEqual(output.read_bytes(), b"known-good\n")
            self.assertEqual(list(output.parent.glob(".routefile.*")), [])


if __name__ == "__main__":
    unittest.main(verbosity=2)
