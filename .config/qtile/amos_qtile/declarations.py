"""Pure-data declarations and a deliberately narrow, window-free refresh operation."""

import copy
import hashlib
import math
from pathlib import Path
import sys
import tomllib

from libqtile.config import DropDown, Match, MatchAny


__source_code__ = sys._getframe().f_code

_MAX_SOURCE_BYTES = 64 * 1024
_MAX_DROPDOWNS = 64
_MAX_MATCHES = 16
_MAX_TEXT = 4096

_DEFAULTS = {
    "x": 0.1, "y": 0.0, "width": 0.8, "height": 0.35, "opacity": 0.9,
    "on_focus_lost_hide": True, "warp_pointer": True,
}
_MATCH_FIELDS = {"title", "wm_class", "wm_instance_class", "role", "wm_type"}
_FIELDS = {"name", "command", "match", *_DEFAULTS}
_REFRESH_FIELDS = {"command", "match"}


def _text(value, label):
    if (not isinstance(value, str) or not value.strip() or "\0" in value
            or len(value) > _MAX_TEXT):
        raise ValueError(
            f"{label} must be a nonempty string, at most {_MAX_TEXT} characters, no NUL")


def _validate(data):
    if not isinstance(data, dict) or set(data) != {"dropdowns"}:
        raise ValueError("declarations must contain only 'dropdowns'")
    if (not isinstance(data["dropdowns"], list)
            or not 1 <= len(data["dropdowns"]) <= _MAX_DROPDOWNS):
        raise ValueError(f"dropdowns must contain 1..{_MAX_DROPDOWNS} entries")
    result = []
    names = set()
    for index, raw in enumerate(data["dropdowns"]):
        label = f"dropdowns[{index}]"
        if not isinstance(raw, dict):
            raise ValueError(f"{label} must be a table")
        unknown = set(raw) - _FIELDS
        missing = {"name", "command", "match"} - set(raw)
        if unknown or missing:
            raise ValueError(
                f"{label}: unknown fields {sorted(unknown)}, missing {sorted(missing)}")
        item = {**_DEFAULTS, **copy.deepcopy(raw)}
        for field in ("name", "command"):
            _text(item[field], f"{label}.{field}")
        if item["name"] in names:
            raise ValueError(f"duplicate dropdown name: {item['name']}")
        names.add(item["name"])
        for field in ("x", "y", "width", "height", "opacity"):
            value = item[field]
            if (type(value) not in (int, float)
                    or (type(value) is float and not math.isfinite(value))):
                raise ValueError(f"{label}.{field} must be a finite number")
            if not 0 <= value <= 1 or (field in ("width", "height") and value == 0):
                raise ValueError(f"{label}.{field} is outside its allowed range")
        for field in ("on_focus_lost_hide", "warp_pointer"):
            if type(item[field]) is not bool:
                raise ValueError(f"{label}.{field} must be a boolean")
        matches = item["match"]
        if not isinstance(matches, list) or not 1 <= len(matches) <= _MAX_MATCHES:
            raise ValueError(f"{label}.match must contain 1..{_MAX_MATCHES} tables")
        for match in matches:
            if not isinstance(match, dict) or not match or set(match) - _MATCH_FIELDS:
                raise ValueError(f"{label}.match allows only {sorted(_MATCH_FIELDS)}")
            for field, value in match.items():
                _text(value, f"{label}.match.{field}")
        result.append(item)
    return {"dropdowns": result}


def load(path=None):
    """Read TOML once; path is for trusted startup/tests, never an IPC argument."""
    path = Path(path) if path is not None else Path(__file__).parent.parent / "scratchpads.toml"
    with path.open("rb") as handle:
        source = handle.read(_MAX_SOURCE_BYTES + 1)
    if len(source) > _MAX_SOURCE_BYTES:
        raise ValueError(f"scratchpad declarations exceed {_MAX_SOURCE_BYTES} bytes")
    data = _validate(tomllib.loads(source.decode("utf-8")))
    return {"source_digest": hashlib.sha256(source).hexdigest(), "data": data}


def _match(alternatives):
    matches = [Match(**fields) for fields in alternatives]
    return matches[0] if len(matches) == 1 else MatchAny(*matches)


def build_matches(snapshot):
    return {item["name"]: _match(item["match"])
            for item in _validate(snapshot["data"])["dropdowns"]}


def build_dropdowns(snapshot):
    result = []
    for item in _validate(snapshot["data"])["dropdowns"]:
        options = {field: item[field] for field in _DEFAULTS}
        result.append(DropDown(item["name"], item["command"],
                               match=_match(item["match"]), **options))
    return result


def capture_sources(config_dir):
    """Fingerprint structural Python, including unknown additions and deletions."""
    root = Path(config_dir).absolute()
    paths = set((root / "amos_qtile").rglob("*.py"))
    if (root / "config.py").exists():
        paths.add(root / "config.py")
    return {str(path): hashlib.sha256(path.read_bytes()).hexdigest()
            for path in sorted(paths)}


def capture_running_sources(config_dir, entry):
    """Reject stale bytecode or edits between module execution and installation."""
    root = Path(config_dir).absolute()
    executed = {str(Path(entry["__file__"]).absolute()): entry["__source_code__"]}
    for module in list(sys.modules.values()):
        path = getattr(module, "__file__", None)
        if path is not None and root / "amos_qtile" in Path(path).absolute().parents:
            executed[str(Path(path).absolute())] = getattr(module, "__source_code__", None)
    sources = capture_sources(root)
    for path, code in executed.items():
        if code is None or path not in sources:
            raise ValueError(f"Missing executed source identity: {path}")
        source = Path(path).read_bytes()
        if (hashlib.sha256(source).hexdigest() != sources[path]
                or compile(source, code.co_filename, "exec", dont_inherit=True,
                           optimize=sys.flags.optimize) != code):
            raise ValueError(f"Python source differs from executed code: {path}")
    if capture_sources(root) != sources:
        raise ValueError("Python sources changed during installation")
    return sources


def _prepare(manager):
    result = {
        "ok": False, "source_digest": None, "scope": "future_unbound_launches",
        "changes": [], "changed_sources": [], "pending": [], "errors": [],
        "requires_full_reload": False, "deferred_full_reload": [],
    }
    state = manager._amos_runtime
    if state.get("reload_active") or state.get("reload_failure"):
        result["errors"].append("Full reload recovery is pending; retry explicit full reload first")
    root = Path(manager.config.file_path).absolute().parent
    sources = capture_sources(root)
    baseline = state["source_digest"]
    if not isinstance(baseline, dict) or not baseline:
        raise ValueError("missing Python source fingerprint baseline")
    result["changed_sources"] = sorted(
        path for path in set(sources) | set(baseline) if sources.get(path) != baseline.get(path)
    )
    if result["changed_sources"]:
        result["requires_full_reload"] = True
        result["errors"].append("Python sources changed; full reload required")
    snapshot = load(root / "scratchpads.toml")
    result["source_digest"] = snapshot["source_digest"]
    old = _validate(state["declarations"]["data"])["dropdowns"]
    new = snapshot["data"]["dropdowns"]
    old_names = [item["name"] for item in old]
    new_names = [item["name"] for item in new]
    scratchpad = manager.groups_map["scratchpad"]
    if old_names != new_names or list(scratchpad._dropdownconfig) != old_names:
        result["requires_full_reload"] = True
        result["errors"].append("Dropdown names/order changed; full reload required")
    previous = {item["name"]: item for item in old}
    removed = sorted(set(previous) - set(new_names))
    if removed:
        result["changes"].extend({"name": name, "fields": ["removed"]} for name in removed)
    for item in new:
        name = item["name"]
        fields = sorted(field for field in item if previous.get(name, {}).get(field) != item[field])
        if not fields:
            continue
        result["changes"].append({"name": name, "fields": fields})
        deferred = sorted(set(fields) - _REFRESH_FIELDS)
        if (name == "stalonetray" and "match" in fields):
            deferred.append("match")
        if deferred:
            result["deferred_full_reload"].append({"name": name, "fields": sorted(deferred)})
        if name in scratchpad._spawned:
            result["pending"].append(name)
    if result["deferred_full_reload"]:
        result["requires_full_reload"] = True
        result["errors"].append("Non-command/matcher changes deferred to full reload")
    if result["pending"]:
        result["errors"].append("Pending launches: " + ", ".join(result["pending"])
                                + "; retry after completion")
    result["ok"] = not result["errors"]
    return result, snapshot, sources


def _error_result(error):
    return {
        "ok": False, "source_digest": None, "scope": "future_unbound_launches",
        "changes": [], "changed_sources": [], "pending": [], "errors": [str(error)],
        "requires_full_reload": False, "deferred_full_reload": [],
    }


def plan(manager):
    """Return a JSONable diff; validation/read failures are explicit errors."""
    try:
        return _prepare(manager)[0]
    except (OSError, ValueError, TypeError, KeyError, AttributeError) as error:
        return _error_result(error)


def apply(manager, expected_digest: str):
    """Swap future-launch configs only, on Qtile's event-loop thread.

    Revalidate before committing; this is not an OS-level transactional filesystem
    snapshot. Existing togglers, windows, hooks and pending launches are untouched.
    """
    result = None
    try:
        if (not isinstance(expected_digest, str) or len(expected_digest) != 64
                or any(char not in "0123456789abcdef" for char in expected_digest)):
            raise ValueError("expected_digest must be a SHA-256 digest from plan")
        result, snapshot, sources = _prepare(manager)
        if result["source_digest"] != expected_digest:
            result["ok"] = False
            result["errors"].append("Stale declaration digest; plan again")
        if not result["ok"]:
            return {**result, "applied": False}
        configs = {dd.name: dd for dd in build_dropdowns(snapshot)}
        root = Path(manager.config.file_path).absolute().parent
        # No mutation until the same bytes and Python baseline have been rechecked.
        if (load(root / "scratchpads.toml")["source_digest"] != expected_digest
                or capture_sources(root) != sources):
            raise ValueError("Sources changed during validation; plan again")
        if result["changes"]:
            manager.groups_map["scratchpad"]._dropdownconfig = configs
        manager._amos_runtime["declarations"] = snapshot
        return {**result, "applied": True, "changed": bool(result["changes"])}
    except (OSError, ValueError, TypeError, KeyError, AttributeError) as error:
        result = _error_result(error) if result is None else {
            **result, "ok": False, "errors": [*result["errors"], str(error)],
        }
        return {**result, "applied": False}
