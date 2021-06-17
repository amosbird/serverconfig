# -*- coding: utf-8 -*-

import re
from xkeysnail.transform import *

# define timeout for multipurpose_modmap
define_timeout(0.4)

# # [Multipurpose modmap] Give a key two meanings. A normal key when pressed and
# # released, and a modifier key when held down with another key. See Xcape,
# # Carabiner and caps2esc for ideas and concept.
define_multipurpose_modmap(
    {
        Key.LEFT_CTRL: [Key.ESC, Key.LEFT_CTRL],
        Key.RIGHT_SHIFT: [Key.ENTER, Key.RIGHT_SHIFT],
        Key.HENKAN: [Key.BACKSPACE, Key.HENKAN],
    }
)

# define_conditional_multipurpose_modmap(re.compile("discord|TelegramDesktop|Wine"),
#     {Key.LEFT_CTRL: [Key.ESC, Key.LEFT_CTRL],
#      Key.RIGHT_SHIFT: [Key.ENTER, Key.RIGHT_SHIFT]}
# )
# # [Conditional multipurpose modmap] Multipurpose modmap in certain conditions,
# # such as for a particular device.
# define_conditional_multipurpose_modmap(lambda wm_class, device_name: device_name.startswith("Microsoft"), {
#    # Left shift is open paren when pressed and released.
#    # Left shift when held down.
#    Key.LEFT_SHIFT: [Key.KPLEFTPAREN, Key.LEFT_SHIFT],

#    # Right shift is close paren when pressed and released.
#    # Right shift when held down.
#    Key.RIGHT_SHIFT: [Key.KPRIGHTPAREN, Key.RIGHT_SHIFT]
# })

# Emacs-like keybindings in non-Emacs applications
define_keymap(
    re.compile("discord"),
    {
        # Cursor
        K("C-b"): with_mark(K("left")),
        K("C-f"): with_mark(K("right")),
        K("C-k"): with_mark(K("up")),
        K("C-j"): with_mark(K("down")),
        K("C-h"): with_mark(K("backspace")),
        K("LM-b"): with_mark(K("RC-left")),
        K("LM-f"): with_mark(K("RC-right")),
        K("C-a"): with_mark(K("home")),
        K("C-e"): with_mark(K("end")),
        K("LM-v"): with_mark(K("page_up")),
        K("C-v"): with_mark(K("page_down")),
        K("C-o"): [K("LShift-end"), K("delete"), set_mark(False)],
        K("C-u"): [K("LShift-home"), K("backspace"), set_mark(False)],
        K("C-i"): K("tab"),
        K("C-d"): [K("delete"), set_mark(False)],
        K("LM-d"): [K("RC-delete"), set_mark(False)],
        K("C-space"): set_mark(True),
        K("C-g"): [K("esc"), set_mark(False)],
        K("LM-backspace"): K("RC-backspace"),
        K("LM-d"): K("RC-delete"),
        K("C-comma"): K("RM-up"),
        K("C-dot"): K("RM-down"),
        K("LM-comma"): K("RC-RM-up"),
        K("LM-dot"): K("RC-RM-down"),
        K("C-s"): [K("RC-f"), set_mark(False)],
        K("LM-j"): [K("end"), K("Shift-slash"), K("enter"), set_mark(False)],
        K("LM-Shift-key_5"): K("RC-h"),
        K("C-key_2"): K("RC-i"),
        K("LM-g"): K("RC-k"),
    },
    "Discord Emacs-like keys",
)

# Emacs-like keybindings in non-Emacs applications
define_keymap(
    re.compile("Wine"),
    {
        # Cursor
        K("C-b"): with_mark(K("left")),
        K("C-f"): with_mark(K("right")),
        K("C-k"): with_mark(K("up")),
        K("C-j"): with_mark(K("down")),
        K("C-h"): with_mark(K("backspace")),
        K("LM-b"): with_mark(K("RC-left")),
        K("LM-f"): with_mark(K("RC-right")),
        K("C-a"): with_mark(K("home")),
        K("C-e"): with_mark(K("end")),
        K("LM-v"): with_mark(K("page_up")),
        K("C-v"): with_mark(K("page_down")),
        K("C-o"): [K("LShift-end"), K("delete"), set_mark(False)],
        K("C-u"): [K("LShift-home"), K("backspace"), set_mark(False)],
        K("C-i"): K("tab"),
        K("C-d"): [K("delete"), set_mark(False)],
        K("LM-d"): [K("RC-delete"), set_mark(False)],
        K("C-space"): set_mark(True),
        K("C-g"): [K("esc"), set_mark(False)],
        K("LM-backspace"): K("RC-backspace"),
        K("LM-d"): K("RC-delete"),
        # wine wechat IME breaks this
        # K("LM-j"): [K("end"), K("enter")],
        K("C-s"): [K("RC-f"), set_mark(False)],
    },
    "Wechat Emacs-like keys",
)

# Emacs-like keybindings in non-Emacs applications
define_keymap(
    re.compile("TelegramDesktop"),
    {
        # Cursor
        K("C-b"): with_mark(K("left")),
        K("C-f"): with_mark(K("right")),
        K("C-k"): with_mark(K("up")),
        K("C-j"): with_mark(K("down")),
        K("C-h"): with_mark(K("backspace")),
        K("LM-b"): with_mark(K("RC-left")),
        K("LM-f"): with_mark(K("RC-right")),
        K("C-a"): with_mark(K("home")),
        K("C-e"): with_mark(K("end")),
        K("LM-v"): with_mark(K("page_up")),
        K("C-v"): with_mark(K("page_down")),
        K("C-o"): [K("LShift-end"), K("delete"), set_mark(False)],
        K("C-u"): [K("LShift-home"), K("backspace"), set_mark(False)],
        K("C-i"): K("tab"),
        K("C-d"): [K("delete"), set_mark(False)],
        K("LM-d"): [K("RC-delete"), set_mark(False)],
        K("LM-backspace"): K("RC-backspace"),
        K("LM-d"): K("RC-delete"),
        K("LC-comma"): K("RC-page_up"),
        K("LC-dot"): K("RC-page_down"),
        K("LM-o"): K("RC-o"),
        K("C-s"): [K("RC-f"), set_mark(False)],
        K("LM-j"): [K("end"), K("LShift-slash"), K("enter"), set_mark(False)],
    },
    "Telegram Emacs-like keys",
)

# Emacs-like keybindings in non-Emacs applications
define_keymap(
    re.compile("Kim"),
    {
        # Cursor
        K("C-b"): with_mark(K("left")),
        K("C-f"): with_mark(K("right")),
        K("C-k"): with_mark(K("up")),
        K("C-j"): with_mark(K("down")),
        K("C-h"): with_mark(K("backspace")),
        K("LM-b"): with_mark(K("RC-left")),
        K("LM-f"): with_mark(K("RC-right")),
        K("C-a"): with_mark(K("home")),
        K("C-e"): with_mark(K("end")),
        K("C-o"): [K("LShift-end"), K("delete"), set_mark(False)],
        K("C-u"): [K("LShift-home"), K("backspace"), set_mark(False)],
        K("C-i"): K("tab"),
        K("C-d"): [K("delete"), set_mark(False)],
        K("LM-d"): [K("RC-delete"), set_mark(False)],
        K("LM-backspace"): K("RC-backspace"),
        K("LM-d"): K("RC-delete"),
        K("LC-comma"): K("RC-page_up"),
        K("LC-dot"): K("RC-page_down"),
        K("LM-o"): K("RC-o"),
        K("C-s"): [K("RC-k"), set_mark(False)],
    },
    "Kim Emacs-like keys",
)
