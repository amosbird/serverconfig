"""A fixed order for this repo's icons at the right end of the tray row.

stalonetray has no order of its own. It hands out slots, lowest free one
first, and an icon keeps its slot until it leaves; whoever docks first is
left of whoever docks next, for as long as the tray runs. A login is a race,
so the row came out in a different order every time.

An indicator therefore only docks into a row that has stopped moving: the
applications that were still starting have their slots by then, and ours
come after them. Each indicator holds a `Tail`, which reads the row off X on
a wall clock boundary, and they dock one after another, in `ORDER`. The
processes never talk to each other. The shared clock is all the agreement
they need, and an indicator that is not running simply leaves a gap.

The same reading keeps the row in order later: when the last slots are not
`ORDER` they all leave it at that same instant, and come back a couple of
checks later, once the tray has given their slots to the icons behind them.
A tray that will not do even that keeps ours wherever they are, since the
slots it gives back are the ones they just freed, and the way out of that is
to start the tray over: it hands its slots out from the beginning again.
"""

import subprocess
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

# The tray, for the one case only a fresh set of slots can settle. Its own
# unit, so that it does not go down with the indicator that restarted it.
TRAY_UNIT = "tray.service"

# Grace between the icons leaving the row and the tray going down with it,
# so that a tray coming back has none of them to hand a slot to.
RESTART_MS = 1000

# A tray that is started over takes every icon in the session with it, so it
# is worth doing once for a row that will not come right, and not again.
RESTART_GAP_S = 600


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


def first_in_row(row):
    """The indicator that speaks for the rest, which is the first one docked."""
    return next((instance for instance in ORDER if instance in row), None)


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

    The indicator hands over the two halves of owning them: `dock` makes the
    icons, `undock` drops them. Hiding an icon and showing it again would be
    the obvious way to leave the row and come back, but this tray forgets an
    icon that does that often enough to matter, and new windows it takes.
    """

    def __init__(self, instance, dock, undock):
        self.instance = instance
        self.dock = dock
        self.undock = undock
        self.row = TrayRow()
        self.docked = False
        self.moving = False
        self.before = None
        self.tried = None
        self.restarted_at = 0.0
        self.schedule()

    def schedule(self):
        GLib.timeout_add(ms_to_next_check(), self.check)

    def check(self):
        self.schedule()
        if self.moving:
            return False
        row = self.row.instances()
        before, self.before = self.before, row
        if row is None:
            # No tray. Drop the icons rather than let them dock themselves
            # into the first slot a tray that comes back offers them.
            if self.docked:
                self.leave()
            return False
        if row != before:
            # The row is still filling up, or a sort is still in flight; a
            # row that is halfway through something says nothing about order.
            return False
        if not self.docked:
            self.arrive()
            return False
        if row.count(self.instance) != SLOTS[self.instance]:
            # The tray drops a dock request now and then. Ask again, in place.
            self.sort()
            return False
        if sorted_row(row):
            self.tried = None
            return False
        if tuple(row) == self.tried:
            # Leaving the row gained nothing, so the slots themselves are in
            # the way. Leave it again, and this time take the tray with it.
            self.sort()
            self.restart_tray(row)
            return False
        self.tried = tuple(row)
        self.sort()
        return False

    def sort(self):
        """Leave the row; the rule for docking brings us back in our place.

        The tray gives the slots of an icon that leaves to the ones behind it,
        but only once it has been gone a while, so this waits out a couple of
        checks rather than dock straight back into the slots it just freed.
        """
        self.moving = True
        GLib.timeout_add(DECIDE_MS, self.leave)

    def leave(self):
        self.undock()
        self.docked = False
        self.moving = False
        return False

    def arrive(self):
        """Take our slots, after those of the indicators before us."""
        self.moving = True
        GLib.timeout_add(STEP_MS * (ORDER.index(self.instance) + 1), self.enter)

    def enter(self):
        self.dock()
        self.docked = True
        self.moving = False
        return False

    def restart_tray(self, row):
        """Start the tray over, for slots that will not come free any other way.

        A tray that holds on to the slots of an icon that left is one nothing
        can get past: the ones we are given back are the ones we just freed.
        A tray that starts fresh hands them out from the beginning again, and
        ours are asked for last, once the applications have theirs.
        """
        if self.instance != first_in_row(row) or time.time() - self.restarted_at < RESTART_GAP_S:
            return
        self.restarted_at = time.time()
        # After the icons are out of the row, so that none of them is handed
        # straight back to the tray that comes up.
        GLib.timeout_add(RESTART_MS, self.start_tray_over)

    def start_tray_over(self):
        subprocess.run(["systemctl", "--user", "restart", TRAY_UNIT], check=False)
        return False
