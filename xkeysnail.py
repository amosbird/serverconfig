# -*- coding: utf-8 -*-

import re
from xkeysnail.transform import *

# define timeout for multipurpose_modmap
define_timeout(1)

# # [Multipurpose modmap] Give a key two meanings. A normal key when pressed and
# # released, and a modifier key when held down with another key. See Xcape,
# # Carabiner and caps2esc for ideas and concept.
define_conditional_multipurpose_modmap(re.compile("discord|TelegramDesktop|Wine"),
    {Key.LEFT_CTRL: [Key.ESC, Key.LEFT_CTRL]}
)

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
define_keymap(re.compile("discord"), {
    # Cursor
    K("C-b"): with_mark(K("left")),
    K("C-f"): with_mark(K("right")),
    K("C-k"): with_mark(K("up")),
    K("C-j"): with_mark(K("down")),
    K("C-h"): with_mark(K("backspace")),
    K("LM-b"): with_mark(K("C-left")),
    K("LM-f"): with_mark(K("C-right")),
    K("C-a"): with_mark(K("home")),
    K("C-e"): with_mark(K("end")),
    K("LM-v"): with_mark(K("page_up")),
    K("C-v"): with_mark(K("page_down")),
    K("C-o"): [K("Shift-end"), K("delete"), set_mark(False)],
    K("C-u"): [K("Shift-home"), K("backspace"), set_mark(False)],
    K("C-i"): K("tab"),
    K("C-d"): [K("delete"), set_mark(False)],
    K("LM-d"): [K("C-delete"), set_mark(False)],
    K("C-space"): set_mark(True),
    K("C-g"): [K("esc"), set_mark(False)],
    K("LM-henkan"): K("C-backspace"),
    K("LM-d"): K("C-delete"),
    K("C-comma"): K("M-up"),
    K("C-dot"): K("M-down"),
    K("LM-comma"): K("C-M-up"),
    K("LM-dot"): K("C-M-down"),

    K("C-s"): [K("C-f"), set_mark(False)],
    K("LM-j"): [K("end"), K("Shift-slash"), K("enter"), set_mark(False)],
    K("LM-Shift-key_5"): K("C-h"),
    K("C-key_2"): K("C-i"),
    K("LM-g"): K("C-k"),
}, "Discord Emacs-like keys")

# Emacs-like keybindings in non-Emacs applications
define_keymap(re.compile("Wine"), {
    # Cursor
    K("C-b"): with_mark(K("left")),
    K("C-f"): with_mark(K("right")),
    K("C-k"): with_mark(K("up")),
    K("C-j"): with_mark(K("down")),
    K("C-h"): with_mark(K("backspace")),
    K("LM-b"): with_mark(K("C-left")),
    K("LM-f"): with_mark(K("C-right")),
    K("C-a"): with_mark(K("home")),
    K("C-e"): with_mark(K("end")),
    K("LM-v"): with_mark(K("page_up")),
    K("C-v"): with_mark(K("page_down")),
    K("C-o"): [K("Shift-end"), K("delete"), set_mark(False)],
    K("C-u"): [K("Shift-home"), K("backspace"), set_mark(False)],
    K("C-i"): K("tab"),
    K("C-d"): [K("delete"), set_mark(False)],
    K("LM-d"): [K("C-delete"), set_mark(False)],
    K("C-space"): set_mark(True),
    K("C-g"): [K("esc"), set_mark(False)],
    K("LM-henkan"): K("C-backspace"),
    K("LM-d"): K("C-delete"),

    K("C-s"): [K("C-f"), set_mark(False)],
    K("LM-j"): [K("end"), K("Shift-slash"), K("enter"), set_mark(False)],
}, "Wechat Emacs-like keys")

# Emacs-like keybindings in non-Emacs applications
define_keymap(re.compile("TelegramDesktop"), {
    # Cursor
    K("C-b"): with_mark(K("left")),
    K("C-f"): with_mark(K("right")),
    K("C-k"): with_mark(K("up")),
    K("C-j"): with_mark(K("down")),
    K("C-h"): with_mark(K("backspace")),
    K("LM-b"): with_mark(K("C-left")),
    K("LM-f"): with_mark(K("C-right")),
    K("C-a"): with_mark(K("home")),
    K("C-e"): with_mark(K("end")),
    K("LM-v"): with_mark(K("page_up")),
    K("C-v"): with_mark(K("page_down")),
    K("C-o"): [K("Shift-end"), K("delete"), set_mark(False)],
    K("C-u"): [K("Shift-home"), K("backspace"), set_mark(False)],
    K("C-i"): K("tab"),
    K("C-d"): [K("delete"), set_mark(False)],
    K("LM-d"): [K("C-delete"), set_mark(False)],
    K("C-space"): set_mark(True),
    K("C-g"): [K("esc"), set_mark(False)],
    K("LM-henkan"): K("C-backspace"),
    K("LM-d"): K("C-delete"),

    K("C-comma"): K("C-page_up"),
    K("C-dot"): K("C-page_down"),
    K("LM-o"): K("C-o"),
    K("C-s"): [K("C-f"), set_mark(False)],
    K("LM-j"): [K("end"), K("Shift-slash"), K("enter"), set_mark(False)],
}, "Telegram Emacs-like keys")
