import re
import subprocess
from threading import Lock

from libqtile import hook, layout, qtile
from libqtile.config import (
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
from libqtile.backend.base import FloatStates
from libqtile.lazy import lazy
from libqtile.utils import send_notification
from libqtile.configurable import Configurable


mod3 = "mod3"
mod4 = "mod4"
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
        x2 = int(self.window.qtile.current_screen.width / 2)
        if not self.visible() or self.window.float_x != x2:
            self.show_float(x2, 0)
        else:
            self.hide()

    def show_float(self, x, y):
        win = self.window
        win.togroup()
        screen = win.qtile.current_screen
        win.opacity = 0.95
        win._float_state = FloatStates.TOP
        win.set_size_floating(int(screen.width / 2), int(screen.height))
        win.toscreen()
        win.set_position_floating(int(x), int(y))
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

    def _spawn(self, x: int):
        with self.spawn_lock:
            if self._spawned:
                return

            hook.subscribe.client_new(self.on_client_new)
            pid = qtile.spawn(
                [
                    "kitty",
                    "-T",
                    "urxvt_scratchpad",
                    "-c",
                    "~/.config/kitty/kitty-mux.conf",
                    "--listen-on",
                    "unix:/tmp/kitty-mux-socket",
                ],
                shell=True,
            )
            self._spawned = (Match(net_wm_pid=pid), x)

    def on_client_new(self, client, *args, **kwargs):
        if self._spawned is None:
            return

        if not self._spawned[0].compare(client):
            return

        hook.unsubscribe.client_new(self.on_client_new)
        self.shell = Shell(client)
        if self._spawned[1] == 2:
            self.shell.toggle_left()
        elif self._spawned[1] == 1:
            self.shell.toggle_right()
        elif self._spawned[1] == 0:
            self.shell.show_tiled()
        hook.subscribe.client_killed(self.on_client_killed)
        self.spawning = False

    def on_client_killed(self, client, *args, **kwargs):
        if self.shell is not None and self.shell.window is client:
            del self.shell
            self.shell = None
            del self._spawned
            self._spawned = None
            hook.unsubscribe.client_killed(self.on_client_killed)

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


keys = [
    Key([mod3], "e", lazy.spawn("rofi -show emoji -modi emoji")),
    Key(
        [mod3],
        "a",
        lazy.spawn("kitty -T float /home/amos/git/work/scripts/insert-cluster.sh"),
    ),
    Key([mod3], "c", lazy.spawn("roficalc")),
    Key([mod3, shift], "c", lazy.spawn("colorinsert")),
    Key([mod3], "w", lazy.spawn("rofiurl")),
    Key([mod3], "f", lazy.spawn("copyq toggle")),
    Key([mod3], "0", lazy.spawn("sleep 0.1 && inputstr 0.0.0.0", shell=True)),
    Key([mod3], "1", lazy.spawn("sleep 0.1 && inputstr 127.0.0.1", shell=True)),
    Key(
        [mod3],
        "2",
        lazy.spawn('joinwemeet "$(xclip -selection clipboard -out)"', shell=True),
    ),
    Key([mod3], "3", lazy.spawn("rofipass")),
    Key([mod3], "4", lazy.spawn("inputstr amosbird@gmail.com")),
    Key([mod3], "r", lazy.spawn("rofidbtbl")),
    Key([mod3], "h", lazy.spawn("rofihosts")),
    Key([mod3], "d", lazy.spawn("dshot | copyq copyImage -", shell=True)),
    Key([mod3, shift], "d", lazy.spawn("dshot | uploadimg -", shell=True)),
    Key([mod3], "t", lazy.spawn("ocr")),
    Key(
        [mod3],
        "p",
        lazy.spawn("flameshot gui -r | pngquant - | copyq copyImage -", shell=True),
    ),
    Key(
        [mod3, shift],
        "p",
        lazy.spawn("flameshot gui -r | pngquant - | uploadimg -", shell=True),
    ),
    Key(
        [mod3],
        "u",
        lazy.spawn("xclip -selection clipboard -out | upload -", shell=True),
    ),
    Key([mod3], "o", lazy.spawn("openclipboard")),
    Key([mod3, shift], "r", lazy.spawn("teiler")),
    Key(
        [mod3], "g", lazy.spawn('url "www.google.com/search?pws=0&gl=us&gws_rd=cr&q="')
    ),
    Key(
        [mod3],
        "k",
        lazy.spawn('inputstr "$(pass show scripts/otp | bash)"', shell=True),
    ),
    Key([mod3], "s", lazy.spawn("/home/amos/git/work/scripts/rofitsearch")),
    Key([mod3], "v", lazy.spawn("rofisound")),
    Key([mod4, shift], "f", lazy.window.toggle_fullscreen()),
    Key([mod4], "f", lazy.window.toggle_floating()),
    Key([mod4], "z", lazy.spawn("lockscreen")),
    Key([ctrl, alt], "Eisu_toggle", lazy.spawn("toggleaudio")),
    Key([ctrl, alt], "1", lazy.group["scratchpad"].dropdown_toggle("ioa")),
    Key([ctrl, alt], "2", lazy.spawn("togglewemeet")),
    Key([ctrl, alt], "3", lazy.spawn("echo p | nc -U /tmp/scrcpy.socket", shell=True)),
    Key([ctrl, alt], "4", lazy.group["scratchpad"].dropdown_toggle("stalonetray")),
    Key([ctrl, alt], "5", lazy.spawn("showobs.sh")),
    Key([ctrl, alt], "8", lazy.group["scratchpad"].dropdown_toggle("chatgpt")),
    Key([ctrl, alt], "9", lazy.group["scratchpad"].dropdown_toggle("stardict")),
    Key([ctrl, alt], "0", lazy.group["scratchpad"].dropdown_toggle("tdesktop")),
    Key([ctrl, alt], "minus", lazy.group["scratchpad"].dropdown_toggle("discord")),
    Key(
        [ctrl, alt],
        "t",
        lazy.spawn("luakit ~/git/rofi-chrome/extension/tab.html 1", shell=True),
    ),
    Key(
        [ctrl, alt],
        "a",
        lazy.spawn("luakit ~/git/rofi-chrome/extension/download.html 1", shell=True),
    ),
    Key([ctrl, alt], "p", lazy.spawn("showpopup.sh")),
    Key([ctrl, alt], "g", lazy.spawn("colorpick")),
    Key([ctrl, alt], "b", lazy.spawn("scanqrcode")),
    Key([ctrl, alt], "s", toggle_shell_left()),
    Key([ctrl, alt], "l", toggle_shell_right()),
    Key([ctrl, alt], "h", show_shell()),
    Key([mod4], "s", lazy.spawn("kitty fish")),
    Key([mod4], "Home", lazy.spawn("movehome")),
    Key([mod4], "End", lazy.spawn("moveend")),
    Key([mod4], "Insert", lazy.spawn("moveinsert")),
    Key([mod4], "Delete", lazy.spawn("lxrandr")),
    Key([ctrl, alt], "r", lazy.spawn("rofi-runner")),
    Key([mod4], "F5", lazy.spawn("redshiftctl")),
    # Key([mod4], "0", lazy.reload_config()),
    Key([ctrl], "Escape", lazy.spawn("dunstctl close-all")),
    Key([ctrl], "Eisu_Toggle", lazy.spawn("dunstctl history-pop")),
    Key([ctrl], "F1", lazy.spawn("pulseaudio-ctl mute")),
    Key([ctrl], "F2", lazy.spawn("pulseaudio-ctl down 3")),
    Key([ctrl], "F3", lazy.spawn("pulseaudio-ctl up 3")),
    Key([ctrl], "F4", lazy.spawn("pavucontrol")),
    Key([ctrl], "F10", lazy.spawn("blueman-manager")),
    Key([], "XF86AudioLowerVolume", lazy.spawn("pulseaudio-ctl down 3")),
    Key([], "XF86AudioRaiseVolume", lazy.spawn("pulseaudio-ctl up 3")),
    Key([], "XF86AudioMute", lazy.spawn("pulseaudio-ctl mute")),
    Key([], "XF86MonBrightnessDown", lazy.spawn("xbacklight -5")),
    Key([], "XF86MonBrightnessUp", lazy.spawn("xbacklight +5")),
    Key([ctrl, alt], "q", lazy.window.kill()),
    Key([ctrl, alt], "j", lazy.layout.next()),
    Key([ctrl, alt], "k", lazy.layout.previous()),
    # Key([ctrl, alt], "Tab", focus_previous_window()),
    Key([mod4], "w", lazy.next_layout()),
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
]

groups = [
    ScratchPad(
        "scratchpad",
        [
            DropDown(
                "ioa",
                "/opt/ioa/bin/iOALinux",
                x=0.26,
                y=0.224,
                opacity=1,
                on_focus_lost_hide=True,
            ),
            DropDown(
                "tdesktop",
                "/opt/telegram/Telegram",
                match=Match(wm_class="TelegramDesktop"),
                x=0.15,
                y=0.1,
                width=0.7,
                height=0.8,
                opacity=1,
                on_focus_lost_hide=False,
            ),
            DropDown(
                "discord",
                "discord",
                match=Match(title=re.compile(r".*Discord$")),
                x=0.15,
                y=0.1,
                width=0.7,
                height=0.8,
                opacity=1,
                on_focus_lost_hide=False,
            ),
            DropDown(
                "chatgpt",
                "runai",
                match=Match(wm_class="chatgpt"),
                x=0.1,
                y=0.1,
                width=0.8,
                height=0.85,
                opacity=1,
                on_focus_lost_hide=False,
            ),
            DropDown(
                "stardict",
                "kitty -T stardict -e dict.sh",
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
                match=Match(title="stalonetray"),
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
    Group("2"),
]

keys.append(Key([ctrl, alt, shift], "2", lazy.window.togroup("2")))

for i in ["w", "e", "d", "f", "v", "n", "i", "o", "c"]:
    groups.append(Group(i))
    keys.append(Key([ctrl, alt], i, lazy.group[i].toscreen()))
    keys.append(Key([ctrl, alt, shift], i, lazy.window.togroup(i)))

dgroups_app_rules = [
    Rule(Match(wm_class="kitty", title="local"), group="e"),
    Rule(Match(wm_class="kitty", title="remote"), group="i"),
    Rule(Match(wm_class="kitty", title="weechat"), group="c"),
    Rule(Match(wm_class="wemeetapp"), group="2"),
    Rule(Match(wm_class="xfreerdp"), group="w"),
    Rule(
        Match(wm_class=re.compile("^[Vv]ivaldi.*")),
        group="f",
    ),
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


@hook.subscribe.client_new
def before_window_created(client):
    if "copyq" in client.get_wm_class():
        client.set_size_floating(2000, 1200)
        client.center()
    elif "kitty" in client.get_wm_class() and client.window.get_name() == "float":
        client.set_size_floating(2000, 1200)
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


@hook.subscribe.client_managed
def after_window_created(client):
    if "chatgpt" in client.get_wm_class():
        client.keep_above()


# @hook.subscribe.layout_change
# def layout_change(layout, group):
#     send_notification("qtile", f"{layout.name} is now on group {group.name}")


class ConditionalBorderWidth(Configurable):
    """
    A class that allows finer control as to which border width is applied to which window.

    To configure the border width, you need to provide two parameters:

      * ``matches``: a list of tuples of (Match rules, border width)
      * ``default``: border width to apply if no matches

    Matches are applied in order and will return a border width as soon as a rule matches.

    It can be used in place of the integer border width layout when defining layouts in your
    config. For example:

    .. code:: python

        from qtile_extras.layout.decorations import ConditionalBorderWidth

        layouts = [
            layout.Columns(
                border_focus_stack=["#d75f5f", "#8f3d3d"],
                border_width=ConditionalBorderWidth(
                    default=2,
                    matches=[(Match(wm_class="vlc"), 0)])
            ),
            ...
        ]

    The above code will default to a border width of 2 but will apply a border width of zero
    for VLC windows.

    """

    defaults = [
        ("default", 0, "Default border width value if no rule is matched"),
        (
            "matches",
            [],
            "List of rules to apply border widths. See docs for more details.",
        ),
    ]

    def __init__(self, **config):
        Configurable.__init__(self, **config)
        self.add_defaults(ConditionalBorderWidth.defaults)

    def get_border_for_window(self, win):
        for rule, value in self.matches:
            if rule.compare(win):
                return value
        return self.default

    # Layouts size windows by subtracting the border width so we
    # need to allow the multiplication to work on the custom class
    # The size will be fixed with the injected window.place code.
    def __mul__(self, other):
        return other * self.default

    __rmul__ = __mul__


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
    if isinstance(borderwidth, ConditionalBorderWidth):
        old = getattr(self, "_old_bw", borderwidth.default)
        newborder = borderwidth.get_border_for_window(self)
        if newborder != old:
            width += old * 2
            width -= newborder * 2
            height += old * 2
            height -= newborder * 2
    else:
        newborder = borderwidth

    self._old_bw = newborder

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


@hook.subscribe.startup_once
def startup():
    from libqtile.backend.x11.window import _Window

    _Window._place = _Window.place
    _Window.place = new_place

    # qtile.debug()
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
floating_layout = layout.Floating(
    # border_width=0,
    border_width=ConditionalBorderWidth(
        default=8, matches=[(Match(title="urxvt_scratchpad"), 0)]
    ),
    # border_focus="#1D1F21",
    # border_normal="#1D1F21",
    border_focus="#FFB300",
    border_normal="#FFB300",
    float_rules=[
        MatchAny(*layout.Floating.default_float_rules)
        & ~Match(wm_class="xfreerdp")
        & ~Match(wm_class="mpv"),
        Match(wm_class="copyq"),
        Match(wm_class="discord"),
        Match(wm_class="TelegramDesktop"),
        # Match(wm_class="wemeetapp"),
    ],
)
auto_fullscreen = False
focus_on_window_activation = "urgent"


# @hook.subscribe.client_urgent_hint_changed
# def client_urgency_change(client):
#     if "discord" in client.get_wm_class():
#         discord = qtile.groups_map["scratchpad"].dropdowns["discord"]
#         win = discord.window
#         win._float_state = FloatStates.TOP
#         win.togroup()
#         win.bring_to_front()
#         discord.shown = True


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


def show_telegram(uri):
    scratchpad = qtile.groups_map["scratchpad"]
    name = "tdesktop"
    if name in scratchpad.dropdowns:
        scratchpad.dropdowns[name].show()
        qtile.spawn(f"/opt/telegram/Telegram -- {uri}")
    else:
        if name in scratchpad._dropdownconfig:
            old_command = scratchpad._dropdownconfig[name].command
            scratchpad._dropdownconfig[name].command = f"{old_command} -- {uri}"
            scratchpad._spawn(scratchpad._dropdownconfig[name])
            scratchpad._dropdownconfig[name].command = old_command


qtile.show_telegram = show_telegram
