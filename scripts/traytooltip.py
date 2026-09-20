"""Tooltips for tray icons, shown only to a pointer that came looking.

At login the pointer is in the middle of the screen, which is where the tray
is, so icons appear right under it; the row also slides sideways under a
still pointer whenever an icon joins or leaves it. GTK reads either as the
pointer arriving on an icon and pops the tooltip unasked, which is a panel of
text beside a row nobody hovered over.

GTK asks before it shows a tooltip, and by then it is clear who moved: the
answer is yes only when the pointer has moved and the icon has not.
"""

import gi

gi.require_version("Gdk", "3.0")
from gi.repository import Gdk


def pointer():
    """Where the pointer is, or None when there is nothing to ask."""
    display = Gdk.Display.get_default()
    if display is None:
        return None
    seat = display.get_default_seat()
    if seat is None:
        return None
    _screen, x, y = seat.get_pointer().get_position()
    return x, y


def place(icon):
    """The icon's place in the tray, or None while it has none."""
    docked, _screen, area, _orientation = icon.get_geometry()
    if not docked:
        return None
    return area.x, area.y, area.width, area.height


class Tooltip:
    """One indicator's tooltip, shared by all of its icons."""

    def __init__(self):
        self.text = ""
        self.seen = {}

    def attach(self, icons):
        """Take fresh icons, and hold their tooltip back until it is asked for."""
        self.seen = {}
        for icon in icons:
            icon.set_has_tooltip(True)
            icon.connect("query-tooltip", self.query)
            self.seen[icon] = (pointer(), place(icon))

    def set(self, text):
        self.text = text

    def query(self, icon, _x, _y, _keyboard, tooltip):
        before, now = self.seen.get(icon), (pointer(), place(icon))
        self.seen[icon] = now
        if before is None or now[1] != before[1] or now[0] == before[0]:
            # The icon came to the pointer, or neither of them moved at all.
            return False
        tooltip.set_text(self.text)
        return True
