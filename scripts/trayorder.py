"""A fixed order for this repo's icons at the right end of the tray row.

stalonetray has no order of its own: an icon takes the first free slot when
it docks, so the row ends up in whatever order the login race and later
restarts produced. Taking an icon out of the middle is no way to fix it
either, because the tray then lays out everything behind it again in an
order of its own.

What the tray does do reliably is append: an icon that leaves the row and
comes back lands at the end. So the indicators sort the row together. Each
holds a `Tail`, which reads the row off X on a wall clock boundary; when the
last slots are not `ORDER`, they all pull their icons out of the row at that
same instant and put them back one after another, in `ORDER`.

The processes never talk to each other. The shared clock is all the
agreement they need, and an indicator that is not running simply leaves a
gap in the order.
"""

import time

import xcffib
import xcffib.xproto
from gi.repository import GLib

# Our icons at the end of the row, left to right, and the slots each covers.
SLOTS = {
    "fcitx5-indicator": 1,
    "bluetooth-indicator": 1,
    "battery-indicator": 1,
    "clock-indicator": 2,
}
ORDER = tuple(SLOTS)

# The row is read on a wall clock boundary, so every indicator sees the same
# row and reaches the same verdict; a read is a couple of dozen X round trips
# over the local socket.
CHECK_MS = 5000

# Grace between the reads and the first icon leaving the row, so that every
# indicator judges the same row rather than one a sort has already started on.
DECIDE_MS = 100

# Spacing between the icons coming back, which is what puts them in ORDER.
STEP_MS = 400


def ms_to_next_check():
    """Delay to the next boundary, the same instant in every indicator."""
    return CHECK_MS - int(time.time() * 1000) % CHECK_MS + 20


def expected_tail(row):
    """The last slots of a sorted row: whoever is docked, in ORDER."""
    tail = []
    for instance in ORDER:
        if row.count(instance):
            tail += [instance] * SLOTS[instance]
    return tail


def sorted_row(row):
    """Whether the row ends in ORDER."""
    tail = expected_tail(row)
    return row[len(row) - len(tail) :] == tail


class TrayRow:
    """The stalonetray icon row, read straight off X."""

    def __init__(self):
        self.conn = None

    def connection(self):
        if self.conn is None:
            self.conn = xcffib.connect()
        return self.conn

    def drop(self):
        conn, self.conn = self.conn, None
        if conn is not None:
            try:
                conn.disconnect()
            except Exception:
                pass

    def instances(self):
        """The row as instance names, left to right, or None when unreadable."""
        try:
            return self.read_instances()
        except Exception:
            # A tray restart takes the connection down with it; the next
            # check reconnects.
            self.drop()
            return None

    def read_instances(self):
        conn = self.connection()
        name = f"_NET_SYSTEM_TRAY_S{conn.pref_screen}".encode()
        atom = conn.core.InternAtom(False, len(name), name).reply().atom
        tray = conn.core.GetSelectionOwner(atom).reply().owner
        if not tray:
            return None
        slots = []
        for window in conn.core.QueryTree(tray).reply().children:
            instance = self.instance_of(conn, window)
            if instance is None:
                continue
            slots.append((conn.core.GetGeometry(window).reply().x, instance))
        return [instance for _x, instance in sorted(slots)]

    @staticmethod
    def instance_of(conn, window):
        """WM_CLASS instance of the icon embedded in a tray slot."""
        candidates = list(conn.core.QueryTree(window).reply().children) + [window]
        for candidate in candidates:
            value = (
                conn.core.GetProperty(
                    False,
                    candidate,
                    xcffib.xproto.Atom.WM_CLASS,
                    xcffib.xproto.Atom.STRING,
                    0,
                    64,
                )
                .reply()
                .value.to_string()
            )
            if value:
                return value.split("\0")[0]
        return None


class Tail:
    """Holds one indicator's icons in their place at the end of the row.

    `undock` drops the indicator's icons; `dock` makes fresh ones. Hiding an
    icon and showing it again would be the obvious way to leave the row and
    come back, but this tray forgets an icon that does that often enough to
    matter, and new windows it always takes.
    """

    def __init__(self, instance, dock, undock):
        self.instance = instance
        self.dock = dock
        self.undock = undock
        self.row = TrayRow()
        self.counts = {}
        self.sorting = False
        self.tried = None
        self.schedule()

    def schedule(self):
        GLib.timeout_add(ms_to_next_check(), self.check)

    def check(self):
        self.schedule()
        if self.sorting:
            return False
        row = self.row.instances()
        if row is None:
            # No tray, or it went away and took our icons with it.
            self.counts = {}
            return False
        counts = {instance: row.count(instance) for instance in ORDER}
        before, self.counts = self.counts, counts
        if counts[self.instance] != SLOTS[self.instance]:
            # The tray drops a dock request now and then, in the churn of a
            # login or of a sort. Ask for our slots again, in our own place.
            self.sort()
            return False
        if any(count not in (0, SLOTS[instance]) for instance, count in counts.items()):
            # Someone is half in the row, so a sort is still in flight.
            return False
        if any(count < before.get(instance, 0) for instance, count in counts.items()):
            # Icons have left the row since the last look, which is either a
            # sort in flight or an indicator that has just stopped.
            return False
        if sorted_row(row):
            self.tried = None
            return False
        if tuple(row) == self.tried:
            # The last sort changed nothing, so a tray that keeps its own
            # order (a different icon gravity, say) is left to it.
            return False
        self.tried = tuple(row)
        self.sort()
        return False

    def sort(self):
        """Leave the row, and come back once those before us have."""
        self.sorting = True
        GLib.timeout_add(DECIDE_MS, self.leave)
        GLib.timeout_add(STEP_MS * (ORDER.index(self.instance) + 1), self.dock_back)

    def leave(self):
        self.undock()
        return False

    def dock_back(self):
        self.dock()
        self.sorting = False
        return False
