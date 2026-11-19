import sys

from libqtile import qtile

from amos_qtile import bindings, declarations, desktop, runtime


__source_code__ = sys._getframe().f_code
snapshot = declarations.load()
globals().update(desktop.build_desktop(snapshot))
keys = bindings.build_keys()
mouse = bindings.build_mouse()

runtime.install(qtile, snapshot, legacy_shell=globals().get("shell"), entry=globals())
# A callback queued by the previous monolithic config still resolves these globals.
pending_inputstr = None
shell = None
