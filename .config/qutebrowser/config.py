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

# import glob

# c.content.user_stylesheets = glob.glob('/home/amos/css/*.user.css')

c.hints.next_regexes = [
    r"^\s*next\s*$",
    r"^\s*more\s*$",
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
    "<Escape>": c.bindings.default["normal"]["<Escape>"]
    + ";; fake-key <Escape> ;; clear-messages ;; jseval --quiet document.getSelection().empty()",
    "<ctrl-Space>": "spawn i3-msg focus right",
    "<Space>": "fake-key <Space>",
    "<ctrl-b>": "navigate prev",
    "<ctrl-f>": "navigate next",
    "<ctrl-p>": "nop",
    "<ctrl-i>": "forward",
    "<ctrl-o>": "back",
    "<ctrl-r>": "undo",
    "<ctrl-alt-u>": "spawn --userscript pbpst",
    "<ctrl+shift+a>": "fake-key <ctrl+a>",
    "I": "tab-close",
    "p": "open -t -- {clipboard}",
    "<Shift-Insert>": "open -t -- {clipboard}",
    "u": "scroll-page 0 -0.5",
    "d": "scroll-page 0 0.5",
    "k": "scroll-page 0 -0.1",
    "j": "scroll-page 0 0.1",
    "i": "hint inputs --first --visible",
    "gi": "enter-mode insert",
    "y": "yank",
    "co": "download-open;; download-remove",
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
    "<ctrl-D>": "rl-delete-char",
    "<ctrl-i>": "completion-item-focus next",
    "<ctrl-J>": "completion-item-focus next",
    "<ctrl-K>": "completion-item-focus prev",
    "<ctrl-O>": "rl-kill-line",
}

c.bindings.commands["prompt"] = {
    "<ctrl-D>": "rl-delete-char",
    "<ctrl-J>": "prompt-item-focus next",
    "<ctrl-K>": "prompt-item-focus prev",
    "<ctrl-O>": "rl-kill-line",
    "<ctrl-i>": "prompt-item-focus next",
}

c.bindings.commands["insert"] = {
    "<Escape>": "leave-mode ;; fake-key <Escape>",
    "<Enter>": "fake-key <Return>",
    "<ctrl+a>": "fake-key <Home>",
    "<ctrl+shift+a>": "fake-key <ctrl+a>",
    "<ctrl+e>": "fake-key <End>",
    "<alt+a>": "fake-key <ctrl+a>",
    "<alt+b>": "fake-key <ctrl+Left>",
    "<alt+f>": "fake-key <ctrl+Right>",
    "<alt+d>": "fake-key <ctrl+Delete>",
    "<alt+backspace>": "fake-key <ctrl+shift+Left>;; fake-key <BackSpace>",
    "<ctrl+w>": "fake-key <ctrl+shift+Left>;; fake-key <BackSpace>",
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

sys.path.append(os.path.join(sys.path[0], "jblock"))
config.source("jblock/jblock/integrations/qutebrowser.py")
