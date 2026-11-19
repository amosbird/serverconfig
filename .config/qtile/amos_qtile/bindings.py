"""Key and mouse builders, with lifecycle-owned deferred input."""

import xcffib.xproto
from libqtile import qtile
from libqtile.backend.x11 import xcbq
from libqtile.config import Click, Drag, Key
from libqtile.lazy import lazy

from . import runtime, scratchpads


super_r = "mod3"
super_l = "mod4"
mod5 = "mod5"
alt = "mod1"
ctrl = "control"
shift = "shift"
lock = "lock"


def key_is_down(keymap, keycode):
    return bool(keymap[keycode // 8] & (1 << (keycode % 8)))


def flush_pending_inputstr(manager, generation):
    state = runtime.get_state(manager)
    if state["input_generation"] != generation:
        return
    state["input_timer"] = None
    if state["pending_inputstr"] is None:
        return
    keymap = manager.core.conn.conn.core.QueryKeymap().reply().keys
    if state["input_generation"] != generation:
        return
    if any(key_is_down(keymap, keycode) for keycode in state["super_r_keycodes"]):
        state["input_timer"] = manager.call_later(
            0.01, flush_pending_inputstr, manager, generation
        )
        return
    _, command, shell = state["pending_inputstr"]
    state["pending_inputstr"] = None
    manager.spawn(command, shell=shell)


def handle_key_release(self, event):
    state = runtime.get_state(self.qtile)
    pending = state["pending_inputstr"]
    if pending is not None and event.detail in pending[0] and state["input_timer"] is None:
        state["input_timer"] = self.qtile.call_later(
            0.01, flush_pending_inputstr, self.qtile, state["input_generation"]
        )


def defer_inputstr(key, value, shell=False):
    command = f'inputstr "{value}"' if shell else ["inputstr", value]

    @lazy.function
    def defer(manager):
        runtime.cancel_input(manager)
        state = runtime.get_state(manager)
        state["super_r_keycodes"] = manager.core.conn.keysym_to_keycode(xcbq.keysyms["super_r"])
        trigger_keycodes = manager.core.conn.keysym_to_keycode(xcbq.keysyms[key])
        state["pending_inputstr"] = (trigger_keycodes, command, shell)

    return defer()


def enable_key_release_events(manager=None):
    manager = qtile if manager is None else manager
    if hasattr(manager.core, "eventmask"):
        manager.core.eventmask |= xcffib.xproto.EventMask.KeyRelease
        manager.core._root.set_attribute(eventmask=manager.core.eventmask)


def build_keys():
    keys = [
        Key([super_r], "e", lazy.spawn("rofi -show emoji -modi emoji")),
        Key(
            [super_r], "a",
            lazy.spawn("kitty -T float /home/amos/git/work/scripts/insert-cluster.sh"),
        ),
        Key([super_r], "c", lazy.spawn("roficalc")),
        Key([super_r, shift], "c", lazy.spawn("colorinsert")),
        Key([super_r], "w", lazy.spawn("rofiurl")),
        Key([super_r], "f", lazy.spawn("copyq toggle")),
        Key([super_r], "0", defer_inputstr("0", "0.0.0.0")),
        Key([super_r], "1", defer_inputstr("1", "127.0.0.1")),
        Key(
            [super_r], "2",
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
            [super_r], "p",
            lazy.spawn("flameshot gui -r | pngquant - | copyq copyImage -", shell=True),
        ),
        Key(
            [super_r, shift], "p",
            lazy.spawn("flameshot gui -r | pngquant - | uploadimg -", shell=True),
        ),
        Key(
            [super_r], "u",
            lazy.spawn("xclip -selection clipboard -out | upload -", shell=True),
        ),
        Key([super_r], "o", lazy.spawn("openclipboard")),
        Key([super_r, shift], "r", lazy.spawn("teiler")),
        Key([super_r], "g", lazy.spawn('url "www.google.com/search?pws=0&gl=us&gws_rd=cr&q="')),
        Key([super_r], "k", defer_inputstr("k", "$(pass show scripts/otp | bash)", shell=True)),
        Key([super_r], "s", lazy.spawn("/home/amos/git/work/scripts/rofitsearch")),
        Key([super_r], "v", lazy.spawn("pavucontrol")),
        Key([ctrl], "F8", lazy.spawn("iwmenu --launcher rofi")),
        Key([ctrl], "F10", lazy.spawn("bzmenu --launcher rofi --interactive")),
        Key([ctrl], "F12", lazy.spawn("reloadtouchpad")),
        Key([super_l, shift], "f", lazy.window.toggle_fullscreen()),
        Key([super_l], "f", lazy.window.toggle_floating()),
        Key([super_l], "z", lazy.spawn("lockscreen")),
        Key([ctrl, alt], "Eisu_toggle", lazy.spawn("toggleaudio")),
        Key([ctrl, alt], "1", scratchpads.toggle_scratchpad("ioa")),
        Key([ctrl, alt], "2", lazy.spawn("togglewemeet")),
        Key([ctrl, alt], "3", lazy.spawn("echo p | nc -U /tmp/scrcpy.socket", shell=True)),
        Key([ctrl, alt], "4", scratchpads.toggle_scratchpad("stalonetray")),
        Key([ctrl, alt], "8", scratchpads.toggle_scratchpad("chatgpt")),
        Key([ctrl, alt], "9", scratchpads.toggle_scratchpad("webchat")),
        Key([ctrl, alt], "0", scratchpads.toggle_scratchpad("tdesktop")),
        Key([ctrl, alt], "minus", scratchpads.toggle_scratchpad("stardict")),
        Key([ctrl, alt], "t", lazy.spawn("rofi-hister")),
        Key([ctrl, alt], "b", scratchpads.toggle_scratchpad("bookmarks")),
        Key(
            [ctrl, alt], "a",
            lazy.spawn(
                "google-chrome-stable "
                "--user-data-dir=/home/amos/.config/google-chrome-downloads "
                "--class=download-manager "
                "--app=chrome-extension://jpgfhlaplofoaempbhliigmjbpofeghk/download.html",
                shell=True,
            ),
        ),
        Key([ctrl, alt], "g", lazy.spawn("colorpick")),
        Key([ctrl, alt], "s", scratchpads.toggle_shell_left()),
        Key([ctrl, alt], "l", scratchpads.toggle_shell_right()),
        Key([ctrl, alt], "h", scratchpads.show_shell()),
        Key([super_l], "s", lazy.spawn("kitty fish")),
        Key([super_l], "Home", lazy.spawn("movehome")),
        Key([super_l], "End", lazy.spawn("moveend")),
        Key([super_l], "Insert", lazy.spawn("moveinsert")),
        Key([super_l], "Delete", lazy.spawn("lxrandr")),
        Key([ctrl, alt], "r", lazy.spawn("rofi-runner")),
        Key([super_l], "0", lazy.function(runtime.refresh_config)()),
        Key([super_l, shift], "0", lazy.reload_config()),
        Key([ctrl], "Escape", lazy.spawn("dunstctl close-all")),
        Key([ctrl], "Eisu_Toggle", lazy.spawn("dunstctl history-pop")),
        Key([ctrl], "F1", lazy.spawn("volume mute")),
        Key([ctrl], "F2", lazy.spawn("volume down")),
        Key([ctrl], "F3", lazy.spawn("volume up")),
        Key([ctrl], "F4", lazy.spawn("microphone-mute")),
        Key([ctrl, shift], "F4", lazy.spawn("bluetooth-profile")),
        Key([], "XF86AudioLowerVolume", lazy.spawn("volume down")),
        Key([], "XF86AudioRaiseVolume", lazy.spawn("volume up")),
        Key([], "XF86AudioMute", lazy.spawn("volume mute")),
        Key([], "XF86MonBrightnessDown", lazy.spawn("xbacklight -5")),
        Key([], "XF86MonBrightnessUp", lazy.spawn("xbacklight +5")),
        Key([ctrl, alt], "q", lazy.window.kill()),
        Key([ctrl, alt], "j", lazy.layout.next()),
        Key([ctrl, alt], "k", lazy.layout.previous()),
        Key([super_l], "w", lazy.next_layout()),
        Key([super_l], "t", lazy.spawn("theme toggle")),
    ]
    keys.append(Key([ctrl, alt, shift], "2", lazy.window.togroup("2")))
    for name in ["w", "e", "d", "f", "v", "n", "i", "o"]:
        keys.append(Key([ctrl, alt], name, lazy.group[name].toscreen()))
        keys.append(Key([ctrl, alt, shift], name, lazy.window.togroup(name)))
    keys.append(Key([ctrl, alt], "c", lazy.group["c"].toscreen()))
    return keys


def build_mouse():
    return [
        Drag(
            [lock], "Button1", lazy.window.set_position_floating(),
            start=lazy.window.get_position(),
        ),
        Drag([lock], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()),
        Click([lock], "Button2", lazy.spawn("ungrab-keyboard")),
    ]
