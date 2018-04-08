require "vertical_tabs"
require "follow_selected"


local unique = require "unique_instance"
unique.open_links_in_new_window = false

local handle = {
    tg = true,
    tencent = true,
    aliim = true,
    mailto = true,
    magnet = true,
}
local webview = require "webview"
local lousy = require "lousy"
local downloads = require "downloads"
downloads.default_dir = os.getenv("HOME") .. "/Downloads"
webview.add_signal("init", function (view)
    view:add_signal("navigation-request", function (v, uri)
                        local scheme = lousy.uri.parse(uri).scheme
                        -- if scheme == "magnet" then
                        --     downloads.aria2(uri, downloads.default_dir)
                        --     return false
                        -- elseif handle[scheme] then
                        if handle[scheme] then
                            luakit.spawn(string.format("%s %q", "xdg-open", uri))
                            return false
                        end
    end)
end)

local window = require "window"
window.add_signal("build", function (w)
                      local widgets, l, r = require "lousy.widget", w.sbar.l, w.sbar.r
                      l.layout:remove(l.layout.children[3])
                      local progress = widgets.progress()
                      r.layout:pack(progress)
                      r.layout:reorder(progress, 1)
end)

local settings = require "settings"
settings.window.search_engines.default = "https://google.com/search?q=%s"
settings.window.search_engines.bing = "https://cn.bing.com/search?q=%s"
settings.window.search_engines.scholar = "https://scholar.google.com/scholar?q=%s"
settings.on["gitter.im"].webview.zoom_level = 100

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

local follow = require "follow"
follow.pattern_maker = follow.pattern_styles.match_label
follow.stylesheet = follow.stylesheet .. [[ #luakit_select_overlay .hint_label { opacity: 0.7; font-size: 16px !important; } ]]

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

local modes = require "modes"

modes.add_binds({"normal","insert"},
    { { "<Control-space>", "Switch to other window.", function (w) luakit.spawn("i3-msg focus right") end } })

local keysym = require "keysym"

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

modes.add_binds("normal",
                {
                    { "^f$", [[Start `follow` mode. Hint all clickable elements
        (as defined by the `follow.selectors.clickable`
        selector) and open links in the current tab.]],
                      function (w)
                          w:set_mode("follow", {
                                         selector = "clickable", evaluator = "click",
                                         func = function (s) w:emit_form_root_active_signal(s) end,
                          })
                          luakit.spawn("fcitx-remote -c")
                    end },

                    -- Open new tab
                    { "^F$", [[Start follow mode. Hint all links (as defined by the
            `follow.selectors.uri` selector) and open links in a new tab.]],
                      function (w)
                          w:set_mode("follow", {
                                         prompt = "background tab", selector = "uri", evaluator = "uri",
                                         func = function (uri)
                                             assert(type(uri) == "string")
                                             w:new_tab(uri, { switch = false, private = w.view.private })
                                         end
                          })
                          luakit.spawn("fcitx-remote -c")
                    end },
                    -- { "<Escape>", function (w)
                    --       w.view:send_key("Escape", {});
                    --       w.view:send_key("Escape", {}, true);
                    --       w:set_prompt();
                    --       w:set_mode();
                    --       w.view:clear_search();
                    --       w.view:eval_js(clear_selection, { no_return = true });
                    -- end },
                    { "h", "Left.", function (w) w.view:send_key("Left", {}) end },
                    { "l", "Right.", function (w) w.view:send_key("Right", {}) end },
                    { "I", "Close current tab (or `[count]` tabs).",
                      function (w, m) for _=1,m.count do w:close_tab() end end, {count=1} },
                    { "<Control-r>", "Undo close tab.", function (w) w:undo_close_tab() end },
                    -- { "m", "Create a bookmark.", function (w) luakit.spawn("rofisearch") end },
                    -- { "s", "Open a new tab via rofi.", function (w) luakit.spawn("rofisearch") end },
                    { "s", "Search via google.", function (w) w:enter_cmd(":tabopen google " ) end },
                    { "S", "Search via scholar.", function (w) w:enter_cmd(":tabopen scholar " ) end },
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
                    { "<Control-f>", "Open the next page in the current tab.",
                      function (w) w.view:eval_js(go_next, { no_return = true }) end },
                    { "<Control-b>", "Open the previous page in the current tab.",
                      function (w) w.view:eval_js(go_prev, { no_return = true }) end },
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
                    { "<Control-u>", function (w)
                          w.view:send_key("Home", {"shift"});
                          w.view:send_key("BackSpace", {});
                          w.view:send_key("BackSpace", {}, true);
                    end },
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
-- vim: et:sw=4:ts=8:sts=4:tw=80
