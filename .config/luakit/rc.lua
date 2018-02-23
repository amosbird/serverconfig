------------------------------------------------------------------------------
-- luakit configuration file, more information at https://luakit.github.io/ --
------------------------------------------------------------------------------

require "lfs"

local unique = require "unique_instance"

unique.open_links_in_new_window = false

-- Set the number of web processes to use. A value of 0 means 'no limit'.
luakit.process_limit = 4
-- Set the cookie storage location
soup.cookies_storage = luakit.data_dir .. "/cookies.db"

-- Load library of useful functions for luakit
local lousy = require "lousy"

-- Load users theme
-- ("$XDG_CONFIG_HOME/luakit/theme.lua" or "/etc/xdg/luakit/theme.lua")
lousy.theme.init(lousy.util.find_config("theme.lua"))
assert(lousy.theme.get(), "failed to load theme")

-- Load users window class
-- ("$XDG_CONFIG_HOME/luakit/window.lua" or "/etc/xdg/luakit/window.lua")
local window = require "window"

-- Load users webview class
-- ("$XDG_CONFIG_HOME/luakit/webview.lua" or "/etc/xdg/luakit/webview.lua")
local webview = require "webview"

local handle = {
    tg = true,
    tencent = true,
    aliim = true,
    mailto = true,
    magnet = true,
}
webview.add_signal("init", function (view)
    view:add_signal("navigation-request", function (v, uri)
                        if handle[lousy.uri.parse(uri).scheme] then
                            luakit.spawn(string.format("%s %q", "xdg-open", uri))
                            return false
                        end
    end)
end)

-- Add luakit;//log/ chrome page
local log_chrome = require "log_chrome"

window.add_signal("build", function (w)
                      local widgets, l, r = require "lousy.widget", w.sbar.l, w.sbar.r

                      -- Left-aligned status bar widgets
                      l.layout:pack(widgets.uri())
                      l.layout:pack(widgets.hist())

                      -- Right-aligned status bar widgets
                      r.layout:pack(widgets.progress())
                      r.layout:pack(widgets.buf())
                      r.layout:pack(log_chrome.widget())
                      r.layout:pack(widgets.ssl())
                      r.layout:pack(widgets.tabi())
                      r.layout:pack(widgets.scroll())
end)

-- Load luakit binds and modes
local modes = require "modes"
local binds = require "binds"

local settings = require "settings"
require "settings_chrome"

settings.window.search_engines.scholar = "https://scholar.google.com/scholar?q=%s"

----------------------------------
-- Optional user script loading --
----------------------------------

-- Add adblock
local adblock = require "adblock"
local adblock_chrome = require "adblock_chrome"

local webinspector = require "webinspector"

-- Add uzbl-like form filling
local formfiller = require "formfiller"

-- Add proxy support & manager
local proxy = require "proxy"

-- Add quickmarks support & manager
local quickmarks = require "quickmarks"

-- Add session saving/loading support
local session = require "session"

-- Add command to list closed tabs & bind to open closed tabs
local undoclose = require "undoclose"

-- Add command to list tab history items
local tabhistory = require "tabhistory"

-- Add greasemonkey-like javascript userscript support
local userscripts = require "userscripts"

-- Add bookmarks support
local bookmarks = require "bookmarks"
local bookmarks_chrome = require "bookmarks_chrome"

-- Add download support
local downloads = require "downloads"
local downloads_chrome = require "downloads_chrome"

-- Set download location
downloads.default_dir = os.getenv("HOME") .. "/Downloads"
downloads.pdf_dir = "pdfs"
downloads.ppt_dir = "ppts"
downloads.doc_dir = "docs"
downloads.add_signal("download-location", function (uri, file)
    if not file or file == "" then
        file = (string.match(uri, "/([^/]+)$")
            or string.match(uri, "^%w+://(.+)")
            or string.gsub(uri, "/", "_")
            or "untitled")
    end
    local ext = file:match(".+%.([^.]+)$");
    if ext == "docx" then ext = "doc" end
    if ext == "pptx" then ext = "ppt" end
    if not ext or not downloads[ext .. "_dir"]
    then return downloads.default_dir .. "/" .. file
    else return os.getenv("HOME") .. "/Documents/" .. downloads[ext .. "_dir"] .. "/" .. file
    end
end)

-- Add automatic PDF downloading and opening
require "viewpdf"

-- Example using xdg-open for opening downloads / showing download folders
downloads.add_signal("open-file",
                     function (file)
                         luakit.spawn(string.format("xdg-open %q", file))
                         return true end)

-- Add vimperator-like link hinting & following
local follow = require "follow"

follow.pattern_maker = follow.pattern_styles.match_label

follow.stylesheet = follow.stylesheet .. [[ #luakit_select_overlay .hint_label { opacity: 0.7; font-size: 16px !important; } ]]

-- Add command history
local cmdhist = require "cmdhist"

cmdhist.history_prev = "<C-p>"
cmdhist.history_next = "<C-n>"

-- Add search mode & binds
require "search"

-- Add ordering of new tabs
local taborder = require "taborder"

taborder.default = taborder.after_current
taborder.default_bg = taborder.after_current

-- Save web history
require "history"
require "history_chrome"

require "help_chrome"
require "binds_chrome"

-- Add command completion
require "completion"

-- Press Control-E while in insert mode to edit the contents of the currently
-- focused <textarea> or <input> element, using `xdg-open`
require "open_editor"

-- NoScript plugin, toggle scripts and or plugins on a per-domain basis.
-- `,ts` to toggle scripts, `,tp` to toggle plugins, `,tr` to reset.
-- If you use this module, don't use any site-specific `enable_scripts` or
-- `enable_plugins` settings, as these will conflict.
--require "noscript"

require "modes"

require "follow_selected"
require "go_input"

local go_next = [=[
(function() {
    function click(e) {
        if (e.href)
            document.location = e.href;
        else {
            var ev = document.createEvent("MouseEvent");
            ev.initMouseEvent("click", true, true, window,
                0, 0, 0, 0, 0, false, false, false, false, 0, null);
            e.dispatchEvent(ev);
        }
    }

    var e = document.querySelector("[rel='next']");
    if (e) // Wow a developer that knows what he's doing!
        click(e);
    else { // Search from the bottom of the page up for a next link.
        var els = Array.from(document.getElementsByTagName("a")).filter(
            elem => elem.offsetWidth || elem.offsetHeight || elem.getClientRects().length);
        var res = "^\\s*(下一页|下一章|下一张|下一篇|下页|后页)>?\\s*$,\\bnext\\b," +
                  "^>$,^(>>|»|→|≫)$,^(>|»),(>|»)$,\\bmore\\b,\\bnewer\\b"
        for (let r of res.split(",").map(r => new RegExp(r, "i"))) {
            var i = els.length;
            while ((e = els[--i])) {
                if (e.text.search(r) > -1) {
                    click(e);
                    return;
                }
            }
        }
    }
})();
]=]

local go_prev = [=[
(function() {
    function click(e) {
        if (e.href)
            document.location = e.href;
        else {
            var ev = document.createEvent("MouseEvent");
            ev.initMouseEvent("click", true, true, window,
                0, 0, 0, 0, 0, false, false, false, false, 0, null);
            e.dispatchEvent(ev);
        }
    }

    var e = document.querySelector("[rel='prev']");
    if (e)
        click(e);
    else {
        var els = Array.from(document.getElementsByTagName("a")).filter(
            elem => elem.offsetWidth || elem.offsetHeight || elem.getClientRects().length);
        var res = "^\\s*<?(上一页|上一章|上一张|上一篇|上页|前页)\\s*$," +
                  "\\b(prev|previous)\\b,^<$,^(<<|«|←|≪)$,^(<|«),(<|«)$,\\bolder\\b"
        for (let r of res.split(",").map(r => new RegExp(r, "i"))) {
            var i = els.length;
            while ((e = els[--i])) {
                if (e.text.search(r) > -1) {
                    click(e);
                    return;
                }
            }
        }
    }
})();
]=]

modes.add_binds("normal", {
                    { "<Control-f>", "Open the next page in the current tab.",
                      function (w) w.view:eval_js(go_next, { no_return = true }) end },
                    { "<Control-b>", "Open the previous page in the current tab.",
                      function (w) w.view:eval_js(go_prev, { no_return = true }) end },
})

require "go_up"

-- Filter Referer HTTP header if page domain does not match Referer domain
require_web_module("referer_control_wm")

require "error_page"

-- Add userstyles loader
require "styles"

-- Hide scrollbars on all pages
require "hide_scrollbars"

-- Add a stylesheet when showing images
require "image_css"

-- Add a new tab page
require "newtab_chrome"

-- Add tab favicons mod
require "tab_favicons"

-- Add :view-source command
require "view_source"

require "vertical_tabs"

local clear_selection = [=[
(function() {
    window.getSelection().removeAllRanges();
})();
]=]

modes.add_binds({"normal","insert"},
    { { "<Control-space>", "Switch to other window.", function (w) luakit.spawn("i3-msg focus right") end } })

local keysym = require "keysym"

modes.add_binds("normal",
                {
                    { "<Escape>", function (w)
                          w.view:send_key("Escape", {});
                          w:set_prompt();
                          w:set_mode();
                          w.view:clear_search();
                          w.view:eval_js(clear_selection, { no_return = true });
                    end },
                    { "I", "Close current tab (or `[count]` tabs).",
                      function (w, m) for _=1,m.count do w:close_tab() end end, {count=1} },
                    { "<Control-r>", "Undo close tab.", function (w) w:undo_close_tab() end },
                    { "<Mod1-g>", function (w) w.view:send_key("k", {"control"}) end },
                    { "m", "Create a bookmark.", function (w) luakit.spawn("rofisearch") end },
                    -- { "s", "Open a new tab via rofi.", function (w) luakit.spawn("rofisearch") end },
                    { "s", "Search via google.", function (w) w:enter_cmd(":tabopen google " ) end },
                    { "S", "Search via scholar.", function (w) w:enter_cmd(":tabopen scholar " ) end },
                    { "h", "Left.", function (w) w.view:send_key("Left", {}) end },
                    { "l", "Right.", function (w) w.view:send_key("Right", {}) end },
                    { "<Space>", "Space.", function (w) w.view:send_key("Space", {}) end },
                    -- { "j", "Down.", function (w) w.view:send_key("Down", {}) end },
                    -- { "k", "Up.", function (w) w.view:send_key("Up", {}) end },

                    -- { "t", "Open a new tab via rofi.", function (w) luakit.spawn("rofiopentab") end },
                    { "d", "Scroll half page down.", function (w) w:scroll{ ypagerel =  0.5 } end },
                    { "u", "Scroll half page up.", function (w) w:scroll{ ypagerel = -0.5 } end },
                    { "J", "Go to next tab.", function (w) w:next_tab() end },
                    { "K", "Go to previous tab.", function (w) w:prev_tab() end },
                    { "D", "none.", function (w) end },
                    { "<Control-v>", "Enter `passthrough` mode, ignores all luakit keybindings.",
                      function (w) w:set_mode("passthrough") end },
                    { "P", [[Open URLs based on the current primary selection contents in the current tab.]],
                      function (w)
                          local uris = {}
                          for uri in string.gmatch(luakit.selection.primary or "", "%S+") do
                              table.insert(uris, uri)
                          end
                          if #uris == 0 then w:notify("Nothing in primary selection...") return end
                          w:navigate(w:search_open(uris[1]))
                          if #uris > 1 then
                              for i=2,#uris do
                                  w:new_tab(w:search_open(uris[i]))
                              end
                          end
                    end },
                    { "p", [[Open a URL based on the current primary selection contents in `[count=1]` new tab(s).]],
                      function (w, _, m)
                          local uri = luakit.selection.primary
                          if not uri then w:notify("No primary selection...") return end
                          for _ = 1, m.count do w:new_tab(w:search_open(uri)) end
                      end, {count = 1} },
})

modes.add_binds("insert", {
                    -- { "<Control-j>", function (w) keysym.send(w, "<Down>"); keysym.send(w, "<Down>", true) end },
                    -- { "<Control-k>", function (w) keysym.send(w, "<Up>") end },
                    { "<Control-j>", function (w) w.view:send_key("Down", {}); w.view:send_key("Down", {}, true) end },
                    { "<Control-k>", function (w) w.view:send_key("Up", {}); w.view:send_key("Up", {}, true) end },
                    { "<Mod1-g>", function (w) w.view:send_key("k", {"control"}) end },
                    -- { "<Control-j>", function (w) luakit.spawn("xdotool --clearmodifiers key Down") end },
                    -- { "<Control-k>", function (w) luakit.spawn("xdotool --clearmodifiers key Up") end },
                    -- { "9", function (w) keysym.send(w, "<Down>") end },
                    -- { "0", function (w) keysym.send(w, "<Up>") end },
                    -- { "<Control-j>", function (w) w.view:eval_js(fakekey, { no_return = true }) end },
                    -- { "<Control-k>", function (w) keysym.send(w, "<Up>") end },
                    { "<Control-a>", function (w) keysym.send(w, "<Home>") end },
                    { "<Control-b>", function (w) keysym.send(w, "<Left>") end },
                    { "<Control-d>", function (w) keysym.send(w, "<Delete>") end },
                    { "<Control-e>", function (w) keysym.send(w, "<End>") end },
                    { "<Control-f>", function (w) keysym.send(w, "<Right>") end },
                    { "<Control-i>", function (w) keysym.send(w, "<Tab>") end },
                    { "<Control-o>", function (w) keysym.send(w, "<Shift-End><Delete>") end },
                    { "<Control-u>", function (w) keysym.send(w, "<Shift-Home><BackSpace>") end },
                    { "<Control-w>", function (w) keysym.send(w, "<Control-BackSpace>") end },
                    { "<Control-Shift-i>", function (w) keysym.send(w, "<Shift-Tab>") end },
                    { "<Mod1-a>", function (w) keysym.send(w, "<Control-a>") end },
                    { "<Mod1-b>", function (w) keysym.send(w, "<Control-Left>") end },
                    { "<Mod1-d>", function (w) keysym.send(w, "<Control-Delete>") end },
                    { "<Mod1-f>", function (w) keysym.send(w, "<Control-Right>") end },
                    { "<Mod1-BackSpace>", function (w) keysym.send(w, "<Control-BackSpace>") end },
})

modes.remap_binds("passthrough", {{ "<Control-v>", "<Escape>" }})
modes.remove_binds({"normal", "insert"}, { "<Control-z>" })

modes.add_binds("completion", {
                    { "<Control-i>", "Select next matching completion item.",
                      function (w) w.menu:move_down() end },
                    { "<Control-Shift-i>", "Select previous matching completion item.",
                      function (w) w.menu:move_up() end },
})

modes.add_binds("command", {
                    { "<Control-j>", "Open completion menu.", function (w) w:set_mode("completion") end },
                    { "<Control-k>", "Open completion menu.", function (w) w:set_mode("completion") end },
                    { "<Control-i>", "Open completion menu.", function (w) w:set_mode("completion") end },
                    { "<Control-Shift-i>", "Open completion menu.", function (w) w:set_mode("completion") end },
})


local select = require "select"
select.label_maker = function (s)
    return s.interleave("fdsrewvcxg", "jkluionmhb")
end

window.add_signal("init", function (w)
                      w:add_signal("mode-changed", function (w, mode)
                                       if mode == "insert" then w:set_prompt("-- INSERT --", { bg = "#4b2" }) end
                      end)
end)

local lru = {}

function lru.new(max_size, max_bytes)

    assert(max_size >= 1, "max_size must be >= 1")
    assert(not max_bytes or max_bytes >= 1,
        "max_bytes must be >= 1")

    -- current size
    local size = 0
    local bytes_used = 0

    -- map is a hash map from keys to tuples
    -- tuple: value, prev, next, key
    -- prev and next are pointers to tuples
    local map = {}

    -- indices of tuple
    local VALUE = 1
    local PREV = 2
    local NEXT = 3
    local KEY = 4
    local BYTES = 5

    -- newest and oldest are ends of double-linked list
    local newest = nil -- first
    local oldest = nil -- last

    local removed_tuple -- created in del(), removed in set()

    -- remove a tuple from linked list
    local function cut(tuple)
        local tuple_prev = tuple[PREV]
        local tuple_next = tuple[NEXT]
        tuple[PREV] = nil
        tuple[NEXT] = nil
        if tuple_prev and tuple_next then
            tuple_prev[NEXT] = tuple_next
            tuple_next[PREV] = tuple_prev
        elseif tuple_prev then
            -- tuple is the oldest element
            tuple_prev[NEXT] = nil
            oldest = tuple_prev
        elseif tuple_next then
            -- tuple is the newest element
            tuple_next[PREV] = nil
            newest = tuple_next
        else
            -- tuple is the only element
            newest = nil
            oldest = nil
        end
    end

    -- insert a tuple to the newest end
    local function put(tuple)
        if not newest then
            newest = tuple
            oldest = tuple
        else
            tuple[NEXT] = newest
            newest[PREV] = tuple
            newest = tuple
        end
    end

    local function del(key, tuple)
        map[key] = nil
        cut(tuple)
        size = size - 1
        bytes_used = bytes_used - (tuple[BYTES] or 0)
        removed_tuple = tuple
    end

    -- removes elemenets to provide enough memory
    -- returns last removed element or nil
    local function makeFreeSpace(bytes)
        while size + 1 > max_size or
            (max_bytes and bytes_used + bytes > max_bytes)
        do
            assert(oldest, "not enough storage for cache")
            del(oldest[KEY], oldest)
        end
    end

    local function get(_, key)
        local tuple = map[key]
        if not tuple then
            return nil
        end
        cut(tuple)
        put(tuple)
        return tuple[VALUE]
    end

    local function set(_, key, value, bytes)
        local tuple = map[key]
        if tuple then
            del(key, tuple)
        end
        if value ~= nil then
            -- the value is not removed
            bytes = max_bytes and (bytes or #value) or 0
            makeFreeSpace(bytes)
            local tuple1 = removed_tuple or {}
            map[key] = tuple1
            tuple1[VALUE] = value
            tuple1[KEY] = key
            tuple1[BYTES] = max_bytes and bytes
            size = size + 1
            bytes_used = bytes_used + bytes
            setNewest(tuple1)
        else
            assert(key ~= nil, "Key may not be nil")
        end
        removed_tuple = nil
    end

    local function delete(_, key)
        return set(_, key, nil)
    end

    local function mynext(_, prev_key)
        local tuple
        if prev_key then
            tuple = map[prev_key][NEXT]
        else
            tuple = newest
        end
        if tuple then
            return tuple[KEY], tuple[VALUE]
        else
            return nil
        end
    end

    -- returns iterator for keys and values
    local function lru_pairs()
        return mynext, nil, nil
    end

    local mt = {
        __index = {
            get = get,
            set = set,
            delete = delete,
            pairs = lru_pairs,
        },
        __pairs = lru_pairs,
    }

    return setmetatable({}, mt)
end

-- window.add_signal("init", function (w)
--                       w.sessions:add_signal("restore", function (state) state[w].tab_visit_order = {} end)
-- end)

-- window.add_signal("init", function (w)
--                       w.sessions:add_signal("save", function (state) state[w].tab_visit_order = {} end)
-- end)

-- local prev_tabs = setmetatable({}, { __mode = "k" })
-- window.add_signal("init", function (w)
--     w.tabs:add_signal("switch-page", function () prev_tabs[w] = w.view end)
-- end)
-- modes.add_binds("all", {{ "<Mod1-m>", function (w) w:goto_tab(w.tabs:indexof(prev_tabs[w])) end }})


-----------------------------
-- End user script loading --
-----------------------------

-- Restore last saved session
local w = (not luakit.nounique) and (session and session.restore())
if w then
    for i, uri in ipairs(uris) do
        w:new_tab(uri, { switch = i == 1 })
    end
else
    -- Or open new window
    window.new(uris)
end
-- vim: et:sw=4:ts=8:sts=4:tw=80
