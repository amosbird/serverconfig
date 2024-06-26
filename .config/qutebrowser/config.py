from qutebrowser.config.configfiles import ConfigAPI  # noqa: F401
from qutebrowser.config.config import ConfigContainer  # noqa: F401
import sys, os

config = config  # type: ConfigAPI # noqa: F821 pylint: disable=E0602,C0103
c = c  # type: ConfigContainer # noqa: F821 pylint: disable=E0602,C0103

# Uncomment this to still load settings configured via autoconfig.yml
config.load_autoconfig()

# config.set('hints.selectors', {'all': ['a[data-hovercard-type="pull_request"]']}, "*.github.com")

# Aliases for commands. The keys of the given dictionary are the
# aliases, while the values are the commands they map to.
# Type: Dict
c.aliases = {"q": "quit", "w": "session-save", "wq": "quit --save"}
c.url.searchengines = {
    "DEFAULT": "https://google.com/search?q={}",
    "google": "https://google.com/search?q={}",
    "github": "https://github.com/search?q={}",
    "searx": "https://searx.me/?q={}",
    "scholar": "https://scholar.google.com/scholar?q={}",
}
c.tabs.last_close = "close"
c.tabs.title.format = "{index}: {current_title}"

c.hints.selectors['inputs'].append('[contenteditable=true]')
c.hints.selectors['links'].append('button')
c.hints.selectors['links'].append('div[class="ytp-miniplayer-ui"][style="display: none;"]')

# file selector
c.fileselect.handler = 'external'

# command for selecting a single file in forms
c.fileselect.single_file.command = ['nnnpicker', '{}']

# command for selecting multiple files in forms
c.fileselect.multiple_files.command = ['nnnpicker', '{}']

# c.statusbar.widgets = ["text:IOA PROXY", "keypress", "url", "scroll", "history", "tabs", "progress"]
c.statusbar.widgets = ["keypress", "url", "scroll", "history", "tabs", "progress"]

# import glob

# c.content.user_stylesheets = glob.glob('/home/amos/css/*.user.css')

c.hints.next_regexes = [
    r"^\s»\s*$",
    r"^\s*next\s*$",
    # r"^\s*more\s*$",
    r"^\s*newer\s*$",
    r"^\s*[>\u2192\u226B]\s*$",
    r"^\s*(>>|\xBB)\s*$",
    r"^\s*continue\s*$",
    r"^\s*下一?页(\s>>)?\s*$",
]
c.hints.prev_regexes = [
    r"^\s*prev(ious)?\s*$",
    r"^\s*back\s*$",
    r"^\s*older\s*$",
    r"^\s*[<\u2190\u226A]\s*$",
    r"^\s*(<<|\xAB)\s*$",
    r"^\s*(<<\s)?上一?页\s*$",
]


# Bindings for normal mode

c.bindings.commands["normal"] = {
    "<Alt-F4>": "tab-close",
    "<Alt-F5>": "undo",
    "<Alt-F6>": "navigate prev",
    "<Alt-F7>": "navigate next",
    "<Alt-F8>": "scroll-to-perc 0",
    "<Alt-F9>": "back",
    "<Alt-F10>": "forward",
    "<F12>": "devtools",
    "<Escape>": c.bindings.default["normal"]["<Escape>"]
    + ";; fake-key <Escape> ;; clear-messages ;; jseval -q document.getSelection().empty()",
    "<ctrl-Space>": "spawn i3-msg focus right",
    "<ctrl-s>": "set-cmd-text /",
    "<Space>": "fake-key <Space>",
    "<alt-a>": "fake-key <ctrl+a>",
    "<ctrl-b>": "navigate prev",
    "<ctrl-f>": "navigate next",
    # "<ctrl-p>": 'set content.proxy system ;; set statusbar.widgets \'["keypress", "url", "scroll", "history", "tabs", "progress"]\'',
    # "<ctrl-shift-p>": 'set content.proxy "http://127.0.0.1:12639" ;; set statusbar.widgets \'["text:IOA PROXY", "keypress", "url", "scroll", "history", "tabs", "progress"]\'',
    "<ctrl-i>": "forward",
    "<ctrl-o>": "back",
    "<ctrl-r>": "undo",
    "<ctrl-,>": "tab-prev",
    "<ctrl-.>": "tab-next",
    "<ctrl-alt-u>": "spawn --userscript pbpst",
    "<ctrl+shift+a>": "fake-key <ctrl+a>",
    "I": "tab-close",
    "p": "open -t -- {clipboard}",
    "<Shift-Insert>": "open -t -- {clipboard}",
    "u": "scroll-page 0 -0.5",
    "d": "scroll-page 0 0.5",
    "k": "scroll-page 0 -0.1",
    "j": "scroll-page 0 0.1",
    "i": "hint inputs --last --visible",
    "gi": "enter-mode insert",
    "go": "spawn translateurl {url:pretty}",
    "y": "yank",
    "co": "download-open ;; download-copy",
    "cc": "download-copy",
    "cC": "download-copy-all",
    "O": "spawn fcitx-remote -c ;; set-cmd-text :open {url:pretty}",
    "T": "spawn fcitx-remote -c ;; set-cmd-text :open -t -r {url:pretty}",
    "t": "spawn fcitx-remote -c ;; set-cmd-text -s :open -t",
    "s": "spawn fcitx-remote -c ;; set-cmd-text -s :open -t google ",
    "S": "spawn fcitx-remote -c ;; set-cmd-text -s :open -t scholar ",
    "gs": "spawn fcitx-remote -c ;; set-cmd-text -s :open -t github ",
}

config.unbind("D")

c.bindings.commands["command"] = {
    "<Alt-F4>": "tab-close",
    "<Alt-F5>": "undo",
    "<Alt-F6>": "navigate prev",
    "<Alt-F7>": "navigate next",
    "<Alt-F8>": "scroll-to-perc 0",
    "<Alt-F9>": "back",
    "<Alt-F10>": "forward",
    "<ctrl-D>": "rl-delete-char",
    "<ctrl-i>": "completion-item-focus next",
    "<ctrl-J>": "completion-item-focus next",
    "<ctrl-K>": "completion-item-focus prev",
    "<ctrl-O>": "rl-kill-line",
}

c.bindings.commands["prompt"] = {
    "<Alt-F4>": "tab-close",
    "<Alt-F5>": "undo",
    "<Alt-F6>": "navigate prev",
    "<Alt-F7>": "navigate next",
    "<Alt-F8>": "scroll-to-perc 0",
    "<Alt-F9>": "back",
    "<Alt-F10>": "forward",
    "<ctrl-D>": "rl-delete-char",
    "<ctrl-J>": "prompt-item-focus next",
    "<ctrl-K>": "prompt-item-focus prev",
    "<ctrl-O>": "rl-kill-line",
    "<ctrl-i>": "prompt-item-focus next",
}

c.bindings.commands["insert"] = {
    "<Alt-F4>": "tab-close",
    "<Alt-F5>": "undo",
    "<Alt-F6>": "navigate prev",
    "<Alt-F7>": "navigate next",
    "<Alt-F8>": "scroll-to-perc 0",
    "<Alt-F9>": "back",
    "<Alt-F10>": "forward",
    "<Escape>": "spawn fcitx-remote -c ;; fake-key <Escape> ;; mode-leave",
    "<Enter>": "fake-key <Return>",
    "<ctrl+a>": "fake-key <Home>",
    "<ctrl+shift+a>": "fake-key <ctrl+a>",
    "<ctrl+e>": "fake-key <End>",
    "<alt+a>": "fake-key <ctrl+a>",
    "<alt+b>": "fake-key <ctrl+Left>",
    "<alt+f>": "fake-key <ctrl+Right>",
    "<alt+d>": "fake-key <ctrl+Delete>",
    # why we are not using ctrl backspace?
    # "<alt+backspace>": "fake-key <ctrl+shift+Left>;; fake-key <BackSpace>",
    # "<ctrl+w>": "fake-key <ctrl+shift+Left>;; fake-key <BackSpace>",
    "<alt+backspace>": "fake-key <ctrl+BackSpace>",
    "<ctrl+w>": "fake-key <ctrl+BackSpace>",
    "<ctrl+d>": "fake-key <Delete>",
    "<ctrl+b>": "fake-key <Left>",
    "<ctrl+f>": "fake-key <Right>",
    "<ctrl+j>": "fake-key <Down>",
    "<ctrl+k>": "fake-key <Up>",
    "<ctrl+i>": "fake-key <Tab>",
    "<ctrl+u>": "fake-key <Shift+Home>;; fake-key <BackSpace>",
    "<ctrl+o>": "fake-key <Shift+End>;; fake-key <Delete>",
    "<ctrl+q>": ":jseval --quiet --file ./ctrlu.js",
    "<ctrl+x>": ":jseval --quiet --file ./ctrlk.js",
}
