from libqtile import qtile

from amos_qtile import bindings, declarations, desktop, runtime


snapshot = declarations.load()
globals().update(desktop.build_desktop(snapshot))
keys = bindings.build_keys()
mouse = bindings.build_mouse()

runtime.install(qtile, snapshot)
