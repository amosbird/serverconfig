import re
import subprocess

from libqtile import hook, layout, qtile
from libqtile.config import Drag, DropDown, Group, Key, Match, Rule, ScratchPad, Screen
from libqtile.core.manager import Qtile
from libqtile.lazy import lazy

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
        screen = win.qtile.current_screen
        win.opacity = 0.95
        win.set_size_floating(int(screen.width / 2), int(screen.height))
        win.toscreen()
        win.set_position_floating(int(x), int(y))
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

    def _spawn(self, x: int):
        hook.subscribe.client_new(self.on_client_new)
        pid = qtile.spawn(
            [
                "kitty",
                "-T",
                "urxvt_scratchpad",
                "bash",
                "-c",
                "env SHELL=/tmp/gentoo/bin/fish tmux -f /home/amos/.tmux/.tmux.conf.gui -L gui new -A -s gui",
            ]
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

    def on_client_killed(self, client, *args, **kwargs):
        if self.shell is not None and self.shell.window is client:
            del self.shell
            self.shell = None
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
    Key([mod3], "a", lazy.spawn("roficalc")),
    Key([mod3], "c", lazy.spawn("colorinsert")),
    Key([mod3], "w", lazy.spawn("rofiurl")),
    Key([mod3], "f", lazy.spawn("copyq toggle")),
    Key([mod3], "0", lazy.spawn("sleep 0.1 && inputstr 0.0.0.0", shell=True)),
    Key([mod3], "1", lazy.spawn("sleep 0.1 && inputstr 127.0.0.1", shell=True)),
    Key([mod3], "2", lazy.spawn('joinwemeet "$(xclip -out)"', shell=True)),
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
    Key([mod3], "u", lazy.spawn("xclip -out | upload -", shell=True)),
    Key([mod3], "o", lazy.spawn("openclipboard")),
    Key([mod3, shift], "r", lazy.spawn("teiler")),
    Key([mod3], "g", lazy.spawn('url "www.google.com/search?q="')),
    Key(
        [mod3],
        "k",
        lazy.spawn('inputstr "$(pass show scripts/otp | bash)"', shell=True),
    ),
    Key([mod3], "s", lazy.spawn('kafkaurl.py "$(xclip -o)"', shell=True)),
    Key([mod3], "v", lazy.spawn("translate")),
    Key([mod4], "z", lazy.spawn("lockscreen")),
    Key([ctrl, alt], "Eisu_toggle", lazy.spawn("toggleaudio")),
    Key([ctrl, alt], "1", lazy.group["scratchpad"].dropdown_toggle("ioa")),
    Key([ctrl, alt], "2", lazy.spawn("togglewemeet")),
    Key([ctrl, alt], "3", lazy.spawn("echo p | nc -U /tmp/scrcpy.socket", shell=True)),
    Key([ctrl, alt], "4", lazy.group["scratchpad"].dropdown_toggle("stalonetray")),
    Key([ctrl, alt], "5", lazy.spawn("showobs.sh")),
    Key([ctrl, alt], "8", lazy.spawn("rofidoc")),
    Key([ctrl, alt], "9", lazy.group["scratchpad"].dropdown_toggle("stardict")),
    Key([ctrl, alt], "0", lazy.group["scratchpad"].dropdown_toggle("telegram")),
    Key([ctrl, alt], "minus", lazy.group["scratchpad"].dropdown_toggle("discord")),
    Key([ctrl, alt], "p", lazy.spawn("showpopup.sh")),
    Key([ctrl, alt], "g", lazy.spawn("colorpick")),
    Key([ctrl, alt], "b", lazy.spawn("scanqrcode")),
    # Key([ctrl, alt], "s", lazy.spawn("toggleshell 2")),
    # Key([ctrl, alt], "l", lazy.spawn("toggleshell 1")),
    # Key([ctrl, alt], "h", lazy.spawn("toggleshell 0")),
    Key([ctrl, alt], "s", toggle_shell_left()),
    Key([ctrl, alt], "l", toggle_shell_right()),
    Key([ctrl, alt], "h", show_shell()),
    Key([mod4], "s", lazy.spawn("kitty fish")),
    Key([mod4], "Home", lazy.spawn("movehome")),
    Key([mod4], "End", lazy.spawn("moveend")),
    Key([mod4], "Insert", lazy.spawn("movesecond")),
    Key([mod4], "Delete", lazy.spawn("lxrandr")),
    Key([ctrl, alt], "r", lazy.spawn("rofi-runner")),
    Key([mod4], "F5", lazy.spawn("redshiftctl")),
    Key([mod4], "0", lazy.reload_config()),
    Key([ctrl], "Escape", lazy.spawn("dunstctl close-all")),
    Key([ctrl], "Eisu_Toggle", lazy.spawn("dunstctl history-pop")),
    Key([mod4], "q", lazy.window.kill()),
    Key([ctrl, alt], "q", lazy.window.kill()),
    Key([ctrl, alt], "j", lazy.layout.next()),
    Key([ctrl, alt], "k", lazy.layout.previous()),
    # Key([ctrl, alt], "Tab", focus_previous_window()),
    Key([mod4], "w", lazy.next_layout()),
    Key([mod4], "f", lazy.window.toggle_fullscreen()),
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
                "/opt/ioa/bin/iOALinux.bin",
                x=0.26,
                y=0.224,
                opacity=1,
                on_focus_lost_hide=True,
            ),
            DropDown(
                "telegram",
                "telegram-desktop",
                match=Match(wm_class="TelegramDesktop"),
                x=0.15,
                y=0.1,
                width=0.7,
                height=0.8,
                opacity=0.99,
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
                opacity=0.99,
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
for i in ["w", "e", "d", "f", "v", "n", "i", "o", "c"]:
    groups.append(Group(i))
    keys.append(Key([ctrl, alt], i, lazy.group[i].toscreen()))
    keys.append(Key([ctrl, alt, shift], i, lazy.window.togroup(i)))

dgroups_app_rules = [
    Rule(Match(wm_class="kitty", title="local"), group="e"),
    Rule(Match(wm_class="kitty", title="remote"), group="i"),
    Rule(Match(wm_class="kitty", title="weechat"), group="c"),
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
    elif "xfreerdp" in client.get_wm_class():
        client.focus()
        client.togroup("w", switch_group=True)
    elif "stalonetray" == client.window.get_name():
        client.set_position_floating(
            client.qtile.current_screen.width * 0.45,
            client.qtile.current_screen.height * 0.45,
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
    if "xfreerdp" in client.get_wm_class():
        client.disable_floating()


# @hook.subscribe.layout_change
# def layout_change(layout, group):
#     send_notification("qtile", f"{layout.name} is now on group {group.name}")


@hook.subscribe.startup_once
def startup():
    subprocess.Popen("startup")


previous_focused = []


@hook.subscribe.client_focus
def client_focused(window):
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


follow_mouse_focus = False
bring_front_click = False
floats_kept_above = True
cursor_warp = False
floating_layout = layout.Floating(
    border_width=0,
    float_rules=[
        *layout.Floating.default_float_rules,
        Match(wm_class="copyq"),
        Match(wm_class="discord"),
        Match(wm_class="discord"),
        Match(wm_class="TelegramDesktop"),
        # Match(wm_class="wemeetapp"),
    ],
)
auto_fullscreen = False
focus_on_window_activation = "focus"
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
