#!/usr/bin/env python3

import json
import pathlib
import re
import shutil
import sys


def stage_assets(source, destination, version):
    if not re.fullmatch(r"(?:0|[1-9][0-9]{0,4})(?:\.(?:0|[1-9][0-9]{0,4})){0,3}", version):
        raise ValueError("Invalid Chrome extension version")
    parts = tuple(int(part) for part in version.split("."))
    if max(parts) > 65535 or not any(parts):
        raise ValueError("Invalid Chrome extension version")

    source = pathlib.Path(source)
    destination = pathlib.Path(destination)
    if source.is_symlink() or any(path.is_symlink() for path in source.rglob("*")):
        raise ValueError("External Links source must not contain symlinks")
    if destination.exists() or destination.is_symlink():
        raise FileExistsError(destination)
    if destination.resolve().is_relative_to(source.resolve()):
        raise ValueError("Asset destination must be outside the source")

    manifest = json.loads((source / "manifest.json").read_text())
    if manifest.get("background", {}).get("service_worker") != "background.js":
        raise ValueError("External Links manifest must use the canonical background.js source")
    background = (source / "background.js").read_text()
    match = re.search(r'importScripts\("([^"]+)"\)', background)
    if not match or match.group(1) != "workspace.js":
        raise ValueError("External Links background must import the canonical workspace.js source")
    workspace = (source / "workspace.js").read_text()
    transport = (source / "playwright-transport.js").read_bytes()
    for text, dependency in ((background, "workspace.js"),
                             (workspace, "playwright-transport.js")):
        if text.count(f'importScripts("{dependency}")') != 1 or \
                text.count(f'require("./{dependency}")') != 1:
            raise ValueError(f"External Links must use exact canonical imports for {dependency}")
    suffix = version.replace(".", "-")
    versioned_workspace = f"workspace-{suffix}.js"
    versioned_worker = f"background-{suffix}.js"
    versioned_transport = f"playwright-transport-{suffix}.js"
    if any((source / name).exists() for name in
           (versioned_workspace, versioned_worker, versioned_transport)):
        raise FileExistsError("Versioned assets already exist in the source")
    versioned_background = background.replace(
        'importScripts("workspace.js")', f'importScripts("{versioned_workspace}")', 1).replace(
        'require("./workspace.js")', f'require("./{versioned_workspace}")')

    versioned_workspace_source = workspace.replace(
        'importScripts("playwright-transport.js")',
        f'importScripts("{versioned_transport}")', 1).replace(
        'require("./playwright-transport.js")', f'require("./{versioned_transport}")', 1)
    shutil.copytree(source, destination)
    (destination / versioned_workspace).write_text(versioned_workspace_source)
    (destination / versioned_transport).write_bytes(transport)
    (destination / versioned_worker).write_text(versioned_background)
    (destination / "background.js").unlink()
    (destination / "workspace.js").unlink()
    (destination / "playwright-transport.js").unlink()
    manifest["version"] = version
    manifest["background"]["service_worker"] = versioned_worker
    (destination / "manifest.json").write_text(json.dumps(manifest, separators=(",", ":")))


if __name__ == "__main__":
    stage_assets(pathlib.Path(sys.argv[1]), pathlib.Path(sys.argv[2]), sys.argv[3])
