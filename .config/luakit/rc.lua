-- luacheck: read globals luakit
require "lfs"
require "unique_instance"
luakit.process_limit = 4
soup.cookies_storage = luakit.data_dir .. "/cookies.db"
local lousy = require "lousy"
lousy.theme.init(lousy.util.find_config("theme.lua"))
assert(lousy.theme.get(), "failed to load theme")
local window = require "window"
local webview = require "webview"
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

window.add_signal("init", function (w)
    w:add_signal("mode-changed", function (_, mode)
        if mode == "insert" then w:set_prompt("-- INSERT --", { bg = "#4b2" }) end
    end)
end)

local settings = require "settings"
require "settings_chrome"

----------------------------------
-- Optional user script loading --
----------------------------------

local adblock = require "adblock"
local adblock_chrome = require "adblock_chrome"
local webinspector = require "webinspector"
local formfiller = require "formfiller"
local proxy = require "proxy"
local quickmarks = require "quickmarks"
local undoclose = require "undoclose"
local tabhistory = require "tabhistory"
local userscripts = require "userscripts"
local bookmarks = require "bookmarks"
local bookmarks_chrome = require "bookmarks_chrome"
local downloads = require "downloads"
local downloads_chrome = require "downloads_chrome"
local search = require "search"
local history = require "history"
local history_chrome = require "history_chrome"
local completion = require "completion"
local open_editor = require "open_editor"
require_web_module("referer_control_wm")
local error_page = require "error_page"
local styles = require "styles"
local hide_scrollbars = require "hide_scrollbars"

-- Add a stylesheet when showing images
-- local image_css = require "image_css"

-- Add a new tab page
local newtab_chrome = require "newtab_chrome"

-- Add tab favicons mod
local tab_favicons = require "tab_favicons"

-- Add :view-source command
local view_source = require "view_source"

require "vertical_tabs"

local handle = {
    tg = true,
    tencent = true,
    aliim = true,
    mailto = true,
    magnet = true,
}
downloads.default_dir = os.getenv("HOME") .. "/Downloads"
webview.add_signal("init", function (view)
    view:add_signal("navigation-request", function (_, uri)
        local scheme = lousy.uri.parse(uri).scheme
        if handle[scheme] then
            luakit.spawn(string.format("%s %q", "xdg-open", uri))
            return false
        end
    end)
end)

settings.window.search_engines.default = "https://google.com/search?q=%s"
settings.window.search_engines.bing = "https://cn.bing.com/search?q=%s"
settings.window.search_engines.scholar = "https://scholar.google.com/scholar?q=%s"
settings.window.search_engines.github = "https://github.com/search?q=%s"

-- Set download location
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

-- downloads.add_signal("download::status", function(dl)
--     if dl.mime_type == "application/pdf" and dl.status == "finished" then
--         downloads.do_open(dl)
--     end
-- end)

-- downloads.add_signal("open-file", function (file, mime_type)
--     if mime_type == "application/pdf" then
--         luakit.spawn(string.format("xdg-open %q", file))
--         return true
--     end
-- end)

local follow = require "follow"
follow.pattern_maker = follow.pattern_styles.match_label
follow.stylesheet = follow.stylesheet ..
    [[ #luakit_select_overlay .hint_label { opacity: 0.7; font-size: 16px !important; } ]]

local select = require "select"
select.label_maker = function (s)
   -- return s.charset("weruiosdfjlkghcvn")
   return s.interleave("fdsrewvcxg", "jkluionmhb")
end

local cmdhist = require "cmdhist"
cmdhist.history_prev = "<C-p>"
cmdhist.history_next = "<C-n>"

local taborder = require "taborder"
taborder.default = taborder.after_current
taborder.default_bg = taborder.after_current

local clear_selection = [=[
(function() {
    window.getSelection().removeAllRanges();
})();
]=]

local go_input = require "go_input"
local go_up = require "go_up"
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

local modes = require "modes"
local binds = require "binds"
local keysym = require "keysym"
modes.add_binds({"normal","insert"},
   { { "<Control-space>", "Switch to other window.", function (_) luakit.spawn("i3-msg focus right") end } })

modes.remove_binds({"normal", "insert"}, { "<Mod1-0>" })
modes.add_binds("all", {
   { "<Escape>", function (w)
      if not w:is_mode("passthrough") then
         -- w.view:send_key("Escape", {});
         -- w.view:send_key("Escape", {}, true);
         w.view:clear_search();
         w.view:eval_js(clear_selection, { no_return = true });
         w:set_prompt();
         w:set_mode()
      end
      return not w:is_mode("passthrough")
  end},
  { "<Mod1-0>", "Enter `passthrough` mode, ignores all luakit keybindings.",
     function (w) w:set_mode("passthrough") end }
})

modes.add_binds("normal", {
    { "^f$", function (w)
        w:set_mode("follow", {
            selector = "clickable", evaluator = "click",
            func = function (s) w:emit_form_root_active_signal(s) end,
        })
        luakit.spawn("fcitx-remote -c")
    end},
    { "^F$", function (w)
        w:set_mode("follow", {
            prompt = "background tab", selector = "uri", evaluator = "uri",
            func = function (uri)
                assert(type(uri) == "string")
                w:new_tab(uri, { switch = false, private = w.view.private })
            end
        })
        luakit.spawn("fcitx-remote -c")
    end},
    { "h", "Left.", function (w) w.view:send_key("Left", {}) end },
    { "l", "Right.", function (w) w.view:send_key("Right", {}) end },
    { "I", "Close current tab (or `[count]` tabs).",
        function (w, m) for _=1,m.count do w:close_tab() end end, {count=1} },
    { "<Control-r>", "Undo close tab.", function (w) w:undo_close_tab() end },
    { "s", "Search via google.", function (w) w:enter_cmd(":tabopen google " ) end },
    { "S", "Search via scholar.", function (w) w:enter_cmd(":tabopen scholar " ) end },
    { "gs", "Search via github.", function (w) w:enter_cmd(":tabopen github " ) end },
    { "d", "Scroll half page down.", function (w) w:scroll{ ypagerel =  0.5 } end },
    { "u", "Scroll half page up.", function (w) w:scroll{ ypagerel = -0.5 } end },
    { "J", "Go to next tab.", function (w) w:next_tab() end },
    { "K", "Go to previous tab.", function (w) w:prev_tab() end },
    { "D", "none.", function (_) end },
    { "<Control-f>", "Open the next page in the current tab.",
        function (w) w.view:eval_js(go_next, { no_return = true }) end },
    { "<Control-b>", "Open the previous page in the current tab.",
        function (w) w.view:eval_js(go_prev, { no_return = true }) end },
    { "P", function (w)
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
    end},
    { "p", function (w, _, m)
        local uri = luakit.selection.primary
        if not uri then w:notify("No primary selection...") return end
        for _ = 1, m.count do w:new_tab(w:search_open(uri)) end
    end, {count = 1}},
    { "gd", [[Open <luakit://downloads> in new tab.]],
        function (w) w:new_tab(downloads_chrome.chrome_page) end },
})

modes.add_binds("insert", {
    { "<Control-j>", function (w) w.view:send_key("Down", {}); w.view:send_key("Down", {}, true) end },
    { "<Control-k>", function (w) w.view:send_key("Up", {}); w.view:send_key("Up", {}, true) end },
    { "<Control-a>", function (w) keysym.send(w, "<Home>") end },
    { "<Control-b>", function (w) keysym.send(w, "<Left>") end },
    { "<Control-d>", function (w) keysym.send(w, "<Delete>") end },
    { "<Control-e>", function (w) keysym.send(w, "<End>") end },
    { "<Control-f>", function (w) keysym.send(w, "<Right>") end },
    { "<Control-i>", function (w) keysym.send(w, "<Tab>") end },
    { "<Control-o>", function (w) keysym.send(w, "<Shift-End><Delete>") end },
    { "<Control-u>", function (w)
        w.view:send_key("Home", {"shift"});
        w.view:send_key("BackSpace", {});
        w.view:send_key("BackSpace", {}, true);
    end},
    { "<Control-w>", function (w) keysym.send(w, "<Control-BackSpace>") end },
    { "<Control-Shift-i>", function (w) keysym.send(w, "<Shift-Tab>") end },
    { "<Mod1-a>", function (w) keysym.send(w, "<Control-a>") end },
    { "<Mod1-b>", function (w) keysym.send(w, "<Control-Left>") end },
    { "<Mod1-d>", function (w) keysym.send(w, "<Control-Delete>") end },
    { "<Mod1-f>", function (w) keysym.send(w, "<Control-Right>") end },
    { "<Mod1-BackSpace>", function (w) keysym.send(w, "<Control-BackSpace>") end },
})

modes.remap_binds("passthrough", {{ "<Mod1-0>", "<Escape>" }})

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

window.new()
