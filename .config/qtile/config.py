import re
import subprocess
from threading import Lock

import xcffib.xproto
from libqtile import hook, layout, qtile
from libqtile.config import (
    Click,
    Drag,
    DropDown,
    Group,
    Key,
    Match,
    MatchAny,
    Rule,
    ScratchPad,
    Screen,
)
from libqtile.core.manager import Qtile
from libqtile.backend.x11 import core as x11_core
from libqtile.backend.x11 import xcbq
from libqtile.lazy import lazy
from libqtile.scratchpad import DropDownToggler
from libqtile.utils import send_notification
from libqtile.configurable import Configurable


super_r = "mod3"
super_l = "mod4"
mod5 = "mod5"
alt = "mod1"
ctrl = "control"
shift = "shift"
lock = "lock"


class Shell:
    def __init__(self, window):
        self.scratchpad_name = "scratchpad"
        self.window = window

    def visible(self):
        if self.window.group is None:
            return False
        return (
            self.window.group.name != self.scratchpad_name
            and self.window.group is self.window.qtile.current_group
            and self.window.has_focus
        )

    def toggle_left(self):
        if not self.visible() or self.window.float_x != 0:
            self.show_float(0, 0)
        else:
            self.hide()

    def toggle_right(self):
        screen = self.window.qtile.current_screen
        if screen is None or not self.window.qtile.screens:
            return
        x2 = int(screen.width / 2)
        if not self.visible() or self.window.float_x != x2:
            self.show_float(x2, 0)
        else:
            self.hide()

    def show_float(self, x, y):
        win = self.window
        screen = win.qtile.current_screen
        if screen is None or not win.qtile.screens:
            return
        win.togroup(win.qtile.current_group.name)
        win.opacity = 0.95
        win.enable_floating()
        win.place(
            int(screen.x + x),
            int(screen.y + y),
            int(screen.width / 2),
            int(screen.height),
            win.qtile.current_group.floating_layout.border_width,
            win.qtile.current_group.floating_layout.border_focus,
            above=True,
        )
        win.bring_to_front()
        win.focus()

    def show_tiled(self):
        win = self.window
        win.qtile.groups_map["h"].toscreen()
        win.togroup("h")
        win.disable_floating()
        win.opacity = 1
        win.focus()

    def hide(self):
        self.window.togroup(self.scratchpad_name)


class ShellHolder:
    def __init__(self):
        self.shell: Shell | None = None
        self._spawned: tuple[Match, int] | None = None
        self.spawn_lock = Lock()

    def _show(self, mode):
        if mode == 2:
            self.shell.toggle_left()
        elif mode == 1:
            self.shell.toggle_right()
        else:
            self.shell.show_tiled()

    def _spawn(self, mode: int):
        with self.spawn_lock:
            if self.shell is not None or self._spawned is not None:
                return
            pid = qtile.spawn(
                [
                    "kitty",
                    "-T",
                    "urxvt_scratchpad",
                    "-c",
                    "/home/amos/.config/kitty/kitty-mux.conf",
                    "--listen-on",
                    "unix:/tmp/kitty-mux-socket",
                ],
                shell=True,
            )
            self._spawned = (Match(net_wm_pid=pid), mode)

    def register(self, window):
        if self.shell is not None or window.name != "urxvt_scratchpad":
            return False
        if self._spawned is not None and not self._spawned[0].compare(window):
            return False
        mode = self._spawned[1] if self._spawned is not None else None
        self.shell = Shell(window)
        self._spawned = None
        if mode is not None:
            self._show(mode)
        return True

    def recover(self, windows):
        found = [window for window in windows if window.name == "urxvt_scratchpad"]
        if not found:
            return
        self.register(next((window for window in found if window.has_focus), found[0]))
        for window in found:
            if window is not self.shell.window:
                window.kill()

    def on_client_killed(self, client, *args, **kwargs):
        if self.shell is not None and self.shell.window is client:
            self.shell = None
            self._spawned = None

    def toggle_left(self):
        if self.shell:
            self.shell.toggle_left()
        else:
            self._spawn(2)

    def toggle_right(self):
        if self.shell:
            self.shell.toggle_right()
        else:
            self._spawn(1)

    def show_shell(self):
        if self.shell:
            self.shell.show_tiled()
        else:
            self._spawn(0)


shell = ShellHolder()


@hook.subscribe.startup
def recover_shell():
    shell.recover(qtile.windows_map.values())


@lazy.function
def toggle_shell_left(qtile: Qtile):
    global shell
    shell.toggle_left()


@lazy.function
def toggle_shell_right(qtile: Qtile):
    global shell
    shell.toggle_right()


@lazy.function
def show_shell(qtile: Qtile):
    global shell
    shell.show_shell()


def toggle_scratchpad(name):
    @lazy.function
    def toggle(qtile: Qtile):
        scratchpad = qtile.groups_map["scratchpad"]
        if name not in scratchpad.dropdowns:
            scratchpad.dropdown_toggle(name)
            return
        dropdown = scratchpad.dropdowns[name]
        if dropdown.window.has_focus:
            dropdown.hide()
            return
        dropdown.show()
        dropdown.window.bring_to_front()
        dropdown.window.focus(warp=True)

    return toggle()


pending_inputstr: tuple[list[int], str | list[str], bool] | None = None
super_r_keycodes: list[int] = []


def key_is_down(keymap, keycode):
    return bool(keymap[keycode // 8] & (1 << (keycode % 8)))


def flush_pending_inputstr(qtile: Qtile):
    global pending_inputstr
    if pending_inputstr is None:
        return

    keymap = qtile.core.conn.conn.core.QueryKeymap().reply().keys
    if any(key_is_down(keymap, keycode) for keycode in super_r_keycodes):
        qtile.call_later(0.01, flush_pending_inputstr, qtile)
        return

    _, command, shell = pending_inputstr
    pending_inputstr = None
    qtile.spawn(command, shell=shell)


def handle_key_release(self, event):
    if pending_inputstr is not None and event.detail in pending_inputstr[0]:
        self.qtile.call_later(0.01, flush_pending_inputstr, self.qtile)


def defer_inputstr(key, value, shell=False):
    command = f'inputstr "{value}"' if shell else ["inputstr", value]

    @lazy.function
    def defer(qtile: Qtile):
        global pending_inputstr, super_r_keycodes
        super_r_keycodes = qtile.core.conn.keysym_to_keycode(xcbq.keysyms["super_r"])
        trigger_keycodes = qtile.core.conn.keysym_to_keycode(xcbq.keysyms[key])
        pending_inputstr = (trigger_keycodes, command, shell)

    return defer()


# shortcut: Qtile has no public release binding; its X11 grab already receives the release event.
x11_core._IGNORED_EVENTS.discard(xcffib.xproto.KeyReleaseEvent)
x11_core.EVENT_TO_HANDLER[xcffib.xproto.KeyReleaseEvent] = "handle_KeyRelease"
x11_core.Core.handle_KeyRelease = handle_key_release


@hook.subscribe.startup
@hook.subscribe.startup_complete
def enable_key_release_events():
    if hasattr(qtile.core, "eventmask"):
        qtile.core.eventmask |= xcffib.xproto.EventMask.KeyRelease
        qtile.core._root.set_attribute(eventmask=qtile.core.eventmask)


keys = [
    Key([super_r], "e", lazy.spawn("rofi -show emoji -modi emoji")),
    Key(
        [super_r],
        "a",
        lazy.spawn("kitty -T float /home/amos/git/work/scripts/insert-cluster.sh"),
    ),
    Key([super_r], "c", lazy.spawn("roficalc")),
    Key([super_r, shift], "c", lazy.spawn("colorinsert")),
    Key([super_r], "w", lazy.spawn("rofiurl")),
    Key([super_r], "f", lazy.spawn("copyq toggle")),
    Key([super_r], "0", defer_inputstr("0", "0.0.0.0")),
    Key([super_r], "1", defer_inputstr("1", "127.0.0.1")),
    Key(
        [super_r],
        "2",
        lazy.spawn('joinwemeet "$(xclip -selection clipboard -out)"', shell=True),
    ),
    Key([super_r], "3", lazy.spawn("rofipass")),
    Key([super_r], "4", defer_inputstr("4", "amosbird@gmail.com")),
    Key([super_r], "r", lazy.spawn("rofidbtbl")),
    Key([super_r], "h", lazy.spawn("rofihosts")),
    Key([super_r], "d", lazy.spawn("dshot | copyq copyImage -", shell=True)),
    Key([super_r, shift], "d", lazy.spawn("dshot | uploadimg -", shell=True)),
    Key([super_r], "t", lazy.spawn("dtinput")),
    Key([super_r], "y", lazy.spawn("ocr")),
    Key(
        [super_r],
        "p",
        lazy.spawn("flameshot gui -r | pngquant - | copyq copyImage -", shell=True),
    ),
    Key(
        [super_r, shift],
        "p",
        lazy.spawn("flameshot gui -r | pngquant - | uploadimg -", shell=True),
    ),
    Key(
        [super_r],
        "u",
        lazy.spawn("xclip -selection clipboard -out | upload -", shell=True),
    ),
    Key([super_r], "o", lazy.spawn("openclipboard")),
    Key([super_r, shift], "r", lazy.spawn("teiler")),
    Key(
        [super_r], "g", lazy.spawn('url "www.google.com/search?pws=0&gl=us&gws_rd=cr&q="')
    ),
    Key(
        [super_r],
        "k",
        defer_inputstr("k", "$(pass show scripts/otp | bash)", shell=True),
    ),
    Key([super_r], "s", lazy.spawn("/home/amos/git/work/scripts/rofitsearch")),
    Key([super_r], "v", lazy.spawn("rofisound")),
    Key([ctrl], "F8", lazy.spawn("iwmenu --launcher rofi")),
    Key([ctrl], "F10", lazy.spawn("bzmenu --launcher rofi --interactive")),
    Key([super_l, shift], "f", lazy.window.toggle_fullscreen()),
    Key([super_l], "f", lazy.window.toggle_floating()),
    Key([super_l], "z", lazy.spawn("lockscreen")),
    Key([ctrl, alt], "Eisu_toggle", lazy.spawn("toggleaudio")),
    Key([ctrl, alt], "1", toggle_scratchpad("ioa")),
    Key([ctrl, alt], "2", lazy.spawn("togglewemeet")),
    Key([ctrl, alt], "3", lazy.spawn("echo p | nc -U /tmp/scrcpy.socket", shell=True)),
    Key([ctrl, alt], "4", toggle_scratchpad("stalonetray")),
    Key([ctrl, alt], "8", toggle_scratchpad("chatgpt")),
    Key([ctrl, alt], "9", toggle_scratchpad("stardict")),
    Key([ctrl, alt], "0", toggle_scratchpad("tdesktop")),
    Key([ctrl, alt], "minus", toggle_scratchpad("webchat")),
    Key([ctrl, alt], "t", lazy.spawn("rofi-hister")),
    Key([ctrl, alt], "b", toggle_scratchpad("bookmarks")),
    Key(
        [ctrl, alt],
        "a",
        lazy.spawn(
            "chromium chrome-extension://jpgfhlaplofoaempbhliigmjbpofeghk/download.html", shell=True
        ),
    ),
    Key([ctrl, alt], "g", lazy.spawn("colorpick")),
    Key([ctrl, alt], "s", toggle_shell_left()),
    Key([ctrl, alt], "l", toggle_shell_right()),
    Key([ctrl, alt], "h", show_shell()),
    Key([super_l], "s", lazy.spawn("kitty fish")),
    Key([super_l], "Home", lazy.spawn("movehome")),
    Key([super_l], "End", lazy.spawn("moveend")),
    Key([super_l], "Insert", lazy.spawn("moveinsert")),
    Key([super_l], "Delete", lazy.spawn("lxrandr")),
    Key([ctrl, alt], "r", lazy.spawn("rofi-runner")),
    Key([super_l], "0", lazy.reload_config()),
    Key([ctrl], "Escape", lazy.spawn("dunstctl close-all")),
    Key([ctrl], "Eisu_Toggle", lazy.spawn("dunstctl history-pop")),
    Key([ctrl], "F1", lazy.spawn("volume mute")),
    Key([ctrl], "F2", lazy.spawn("volume down")),
    Key([ctrl], "F3", lazy.spawn("volume up")),
    Key([ctrl], "F4", lazy.spawn("bluetooth-profile")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("volume down")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("volume up")),
    Key([], "XF86AudioMute", lazy.spawn("volume mute")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("xbacklight -5")),
    Key([], "XF86MonBrightnessUp", lazy.spawn("xbacklight +5")),
    Key([ctrl, alt], "q", lazy.window.kill()),
    Key([ctrl, alt], "j", lazy.layout.next()),
    Key([ctrl, alt], "k", lazy.layout.previous()),
    # Key([ctrl, alt], "Tab", focus_previous_window()),
    Key([super_l], "w", lazy.next_layout()),
    Key([super_l], "t", lazy.spawn("theme toggle")),
]

mouse = [
    Drag(
        [lock],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [lock], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([lock], "Button2", lazy.spawn("ungrab-keyboard")),
]

scratchpad_matches = {
    "ioa": Match(title="iOA"),
    "tdesktop": Match(wm_class="TelegramDesktop"),
    "webchat": Match(wm_class="webchat"),
    "chatgpt": Match(wm_class="chatgpt"),
    "stardict": Match(title="stardict"),
    "stalonetray": Match(title="stalonetray"),
    "bookmarks": Match(wm_class="jpgfhlaplofoaempbhliigmjbpofeghk__bookmarks.html"),
}


groups = [
    ScratchPad(
        "scratchpad",
        [
            DropDown(
                "ioa",
                "/opt/ioa/bin/iOALinux",
                match=scratchpad_matches["ioa"],
                x=0.26,
                y=0.224,
                opacity=1,
                on_focus_lost_hide=True,
            ),
            DropDown(
                "tdesktop",
                "/opt/telegram/Telegram",
                match=scratchpad_matches["tdesktop"],
                x=0.15,
                y=0.1,
                width=0.7,
                height=0.8,
                opacity=1,
                on_focus_lost_hide=False,
            ),
            DropDown(
                "webchat",
                "runchat",
                match=scratchpad_matches["webchat"],
                x=0.1,
                y=0.1,
                width=0.8,
                height=0.85,
                opacity=1,
                on_focus_lost_hide=False,
            ),
            DropDown(
                "chatgpt",
                "runai",
                match=scratchpad_matches["chatgpt"],
                x=0.1,
                y=0.1,
                width=0.8,
                height=0.85,
                opacity=1,
                on_focus_lost_hide=False,
            ),
            DropDown(
                "bookmarks",
                "bookmark-manager",
                match=scratchpad_matches["bookmarks"],
                x=0.1,
                y=0.05,
                width=0.8,
                height=0.9,
                opacity=1,
                on_focus_lost_hide=False,
            ),
            DropDown(
                "stardict",
                "kitty -T stardict -e dict.sh",
                match=scratchpad_matches["stardict"],
                x=0.25,
                y=0.1,
                width=0.5,
                height=0.8,
                opacity=0.75,
                on_focus_lost_hide=True,
            ),
            DropDown(
                "stalonetray",
                "stalonetray --icon-size=96 --kludges=force_icons_size",
                match=scratchpad_matches["stalonetray"],
                x=0.45,
                y=0.45,
                # width=0.1,
                # height=0.1,
                # opacity=1,
                on_focus_lost_hide=True,
            ),
        ],
    ),
    Group("h"),
    Group("2", layout="max"),
]

keys.append(Key([ctrl, alt, shift], "2", lazy.window.togroup("2")))

for i in ["w", "e", "d", "f", "v", "n", "i", "o", "c"]:
    groups.append(Group(i))
    keys.append(Key([ctrl, alt], i, lazy.group[i].toscreen()))
    keys.append(Key([ctrl, alt, shift], i, lazy.window.togroup(i)))

dgroups_app_rules = [
    Rule(Match(wm_class="kitty", title="local"), group="e"),
    Rule(Match(wm_class="kitty", title="work"), group="v"),
    Rule(Match(wm_class="kitty", title="remote"), group="i"),
    Rule(Match(wm_class="kitty", title="weechat"), group="c"),
    Rule(Match(wm_class="wemeetapp"), group="2"),
    Rule(Match(wm_class="xfreerdp"), group="w"),
    Rule(Match(wm_class="Google-chrome"), group="f"),
]

border = dict(border_width=0)

layouts = [
    # layout.Bsp(**border),
    # layout.Stack(num_stacks=2),
    # layout.Matrix(),
    layout.MonadTall(**border),
    # layout.MonadWide(),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    # layout.Zoomy(),
    layout.Max(),
]

screens = [Screen()]


@hook.subscribe.startup
def recover_scratchpad_dropdowns():
    for window in qtile.windows_map.values():
        register_scratchpad_window(window)


def register_scratchpad_window(window, hide=True):
    scratchpad = qtile.groups_map["scratchpad"]
    if any(dropdown.window is window for dropdown in scratchpad.dropdowns.values()):
        return
    for name, match in scratchpad_matches.items():
        if name in scratchpad.dropdowns or not match.compare(window):
            continue
        config = next(config for config in groups[0].dropdowns if config.name == name)
        scratchpad.dropdowns[name] = DropDownToggler(window, scratchpad.name, config)
        if hide:
            scratchpad.dropdowns[name].hide()
        else:
            scratchpad.dropdowns[name].show()
        return


def show_scratchpad(name):
    scratchpad = qtile.groups_map["scratchpad"]
    if name in scratchpad.dropdowns:
        dropdown = scratchpad.dropdowns[name]
        dropdown.show()
        dropdown.window.bring_to_front()
        dropdown.window.focus(warp=True)


qtile.show_scratchpad = show_scratchpad


@hook.subscribe.client_new
def before_window_created(client):
    if "copyq" in client.get_wm_class():
        client.enable_floating()
        client.set_size_floating(2000, 1200)
        client.center()
    elif "kitty" in client.get_wm_class() and client.window.get_name() == "float":
        client.enable_floating()
        client.set_size_floating(2000, 1200)
        client.center()
    elif "kitty" in client.get_wm_class() and client.window.get_name() == "dtpick":
        client.enable_floating()
        client.set_size_floating(400, 120)
        client.center()
    elif "xfreerdp" in client.get_wm_class():
        client.focus()
        client.togroup("w", switch_group=True)
    elif "stalonetray" == client.window.get_name():
        client.set_position_floating(
            int(client.qtile.current_screen.width * 0.45),
            int(client.qtile.current_screen.height * 0.45),
        )
    elif "urxvt_scratchpad" == client.window.get_name():
        # client.togroup("scratchpad", switch_group=False)
        with open("/tmp/urxvt_scratchpad", "w") as file:
            file.write(str(client.wid))
    # elif "stalonetray" in client.get_wm_class():
    #     client.set_size_floating(500, 200)
    #     client.center()
    # doesn't work


@hook.subscribe.client_killed
def window_killed(client):
    shell.on_client_killed(client)


@hook.subscribe.client_managed
def after_window_created(client):
    if shell.register(client):
        return
    if scratchpad_matches["bookmarks"].compare(client):
        register_scratchpad_window(client, hide=False)
    else:
        register_scratchpad_window(client)
    if scratchpad_matches["bookmarks"].compare(client):
        return
    if "Google-chrome" in client.get_wm_class() and client.get_wm_role() == "pop-up":
        screen = client.qtile.current_screen
        client.enable_floating()
        client.set_size_floating(int(screen.width * 0.7), int(screen.height * 0.8))
        client.set_position_floating(
            int(screen.x + screen.width * 0.15),
            int(screen.y + screen.height * 0.1),
        )
    elif "chatgpt" in client.get_wm_class():
        client.keep_above()


# @hook.subscribe.layout_change
# def layout_change(layout, group):
#     send_notification("qtile", f"{layout.name} is now on group {group.name}")


class ConditionalBorderColor(str):
    def __new__(cls, default, matches):
        color = super().__new__(cls, default)
        color.default = default
        color.matches = matches
        return color

    def get_border_for_window(self, win):
        for rule, value in self.matches:
            if rule.compare(win):
                return value
        return self.default


class ConditionalBorderWidth(int):
    def __new__(cls, default, matches):
        border_width = super().__new__(cls, default)
        border_width.default = default
        border_width.matches = matches
        return border_width

    def get_border_for_window(self, win):
        for rule, value in self.matches:
            if rule.compare(win):
                return value
        return self.default


def new_place(
    self,
    x,
    y,
    width,
    height,
    borderwidth,
    bordercolor,
    above=False,
    margin=None,
    respect_hints=False,
):
    if hasattr(borderwidth, "get_border_for_window"):
        old = getattr(self, "_old_bw", borderwidth.default)
        if not isinstance(old, int):
            old = borderwidth.default
        newborder = borderwidth.get_border_for_window(self)
        if newborder != old:
            width += old * 2
            width -= newborder * 2
            height += old * 2
            height -= newborder * 2
    else:
        newborder = borderwidth

    self._old_bw = newborder
    if hasattr(bordercolor, "get_border_for_window"):
        bordercolor = bordercolor.get_border_for_window(self)

    self._place(
        x,
        y,
        width,
        height,
        newborder,
        bordercolor,
        above=above,
        margin=margin,
        respect_hints=respect_hints,
    )


@hook.subscribe.startup
def patch_window_place():
    from libqtile.backend.x11.window import _Window

    if not hasattr(_Window, "_place"):
        _Window._place = _Window.place
    _Window.place = new_place


@hook.subscribe.startup_once
def startup():
    subprocess.Popen("startup")


previous_focused = []


@hook.subscribe.client_focus
def client_focused(window):
    if "urxvt_scratchpad" == window.name:
        window.border_width = 0

    global previous_focused
    if len(previous_focused) < 2:
        previous_focused.append(window)
    elif previous_focused[1] != window:
        previous_focused[0] = previous_focused[1]
        previous_focused[1] = window
    # logger.info(f"FOCUSED {window}, {previous_focused}")


@lazy.function
def focus_previous_window(qtile: Qtile):
    global previous_focused
    if len(previous_focused) == 2:
        group = previous_focused[0].group
        qtile.current_screen.set_group(group)
        # logger.info(f"FOCUS PREVIOUS {previous_focused[0]}")
        group.focus(previous_focused[0])


# @hook.subscribe.focus_change
# def focus_changed():
#     window = qtile.current_window
#     # send_notification("qtile", f"Focus changed.")
#     if "urxvt_scratchpad" == window.name:
#         window.border_width = 0
#         send_notification("qtile", "Focus changed.")


# @hook.subscribe.group_window_add
# def group_window_add(group, window):
#     if "urxvt_scratchpad" == window.name:
#         window.border_width = 0
#         send_notification("qtile", f"Window {window.name} added to {group.name}")


follow_mouse_focus = False
bring_front_click = "floating_only"
floats_kept_above = True
cursor_warp = False
floating_border_colors = ConditionalBorderColor(
    default="#FFB300",
    matches=[
        (scratchpad_matches["ioa"], "#F94144"),
        (scratchpad_matches["tdesktop"], "#70A288"),
        (scratchpad_matches["webchat"], "#2A9D8F"),
        (scratchpad_matches["chatgpt"], "#84A98C"),
        (scratchpad_matches["bookmarks"], "#4F6D7A"),
        (scratchpad_matches["stardict"], "#E83E8C"),
        (scratchpad_matches["stalonetray"], "#F2CC8F"),
        (Match(wm_class="Google-chrome", role="pop-up"), "#6A994E"),
        (Match(wm_class="copyq"), "#E9C46A"),
    ],
)

floating_layout = layout.Floating(
    border_width=ConditionalBorderWidth(
        default=8,
        matches=[
            (Match(title="urxvt_scratchpad"), 0),
            (Match(wm_class="flameshot"), 0),
            (Match(wm_class="kitty", title="float"), 0),
            (Match(wm_class="kitty", title="dtpick"), 0),
        ],
    ),
    border_focus=floating_border_colors,
    border_normal=floating_border_colors,
    float_rules=[
        MatchAny(*layout.Floating.default_float_rules)
        & ~Match(wm_class="xfreerdp")
        & ~Match(wm_class="mpv"),
        Match(wm_class="Google-chrome", role="pop-up"),
        Match(wm_class="copyq"),
        Match(wm_class="TelegramDesktop"),
        Match(wm_class="kitty", title="dtpick"),
        # Match(wm_class="wemeetapp"),
    ],
)
auto_fullscreen = False
focus_on_window_activation = "urgent"


reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = False

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
