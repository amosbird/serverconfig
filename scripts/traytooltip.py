"""Tooltips for tray icons, shown only to a pointer that came looking.

At login the pointer is in the middle of the screen, which is where the tray
is, so icons appear right under it; the row also slides sideways under a
still pointer whenever an icon joins or leaves it. GTK reads either as the
pointer arriving on an icon and pops the tooltip unasked, which is a panel of
text beside a row nobody hovered over.

GTK asks before it shows a tooltip, and by then it is clear who moved. A
pointer that moves on an icon that holds still is one that came looking, and
the answer stays yes until an icon moves under a pointer that holds still.
It has to stay: GTK asks again, with nothing having moved since, and that
second answer is the one that decides whether the tooltip is shown at all.
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
            self.seen[icon] = (pointer(), place(icon), False)

    def set(self, text):
        self.text = text

    def query(self, icon, _x, _y, _keyboard, tooltip):
        position, where = pointer(), place(icon)
        show = self.asked_for(self.seen.get(icon), position, where)
        self.seen[icon] = (position, where, show)
        if not show:
            return False
        tooltip.set_text(self.text)
        return True

    @staticmethod
    def asked_for(before, position, where):
        """Whether the pointer came to the icon rather than the icon to it."""
        if before is None:
            return False
        was_position, was_where, show = before
        if where != was_where:
            return False  # the icon moved under the pointer
        if position != was_position:
            return True  # the pointer moved on the icon
        return show  # neither moved, so GTK is asking a second time
