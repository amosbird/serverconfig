--------------------------
-- Default luakit theme --
--------------------------

local theme = {}

-- Default settings
-- theme.font = "18px Inconsolata"
theme.font = "18px monospace" -- inconsistency of lua w:notify(w.sbar.ebox.height..' '..w.ibar.ebox.height)
theme.fg   = "#ddd"
theme.bg   = "#333"

-- Genaral colours
theme.success_fg = "#0f0"
theme.loaded_fg  = "#33AADD"
theme.error_fg = "#ddd"
theme.error_bg = "#449"

-- Warning colours
theme.warning_fg = "#449"
theme.warning_bg = "#ddd"

-- Notification colours
theme.notif_fg = "#444"
theme.notif_bg = "#ddd"

-- Menu colours
theme.menu_fg                   = "#000"
theme.menu_bg                   = "#fff"
theme.menu_selected_fg          = "#000"
theme.menu_selected_bg          = "#FF0"
theme.menu_title_bg             = "#fff"
theme.menu_primary_title_fg     = "#f00"
theme.menu_secondary_title_fg   = "#666"

theme.menu_disabled_fg = "#999"
theme.menu_disabled_bg = theme.menu_bg
theme.menu_enabled_fg = theme.menu_fg
theme.menu_enabled_bg = theme.menu_bg
theme.menu_active_fg = "#060"
theme.menu_active_bg = theme.menu_bg

-- Proxy manager
theme.proxy_active_menu_fg      = '#000'
theme.proxy_active_menu_bg      = '#FFF'
theme.proxy_inactive_menu_fg    = '#888'
theme.proxy_inactive_menu_bg    = '#FFF'

-- Statusbar specific
theme.sbar_fg         = "#ddd"
theme.sbar_bg         = "#444"

-- Downloadbar specific
theme.dbar_fg         = "#fff"
theme.dbar_bg         = "#000"
theme.dbar_error_fg   = "#F00"

-- Input bar specific
theme.ibar_fg           = "#000"
theme.ibar_bg           = "rgba(0,0,0,0)"

-- Tab label
theme.tab_fg            = "#ddd"
theme.tab_bg            = "#444"
theme.tab_hover_bg      = "#292929"
theme.tab_ntheme        = "#ddd"
theme.selected_fg       = "#333"
theme.selected_bg       = "#8df"
theme.selected_ntheme   = "#ddd"
theme.loading_fg        = "#33AADD"
theme.loading_bg        = "#444"

theme.selected_private_tab_bg = "#3d295b"
theme.private_tab_bg    = "#22254a"

-- Trusted/untrusted ssl colours
theme.trust_fg          = "#090"
theme.notrust_fg        = "#c44"

-- General colour pairings
theme.ok = { fg = "#444", bg = "#ddd" }
theme.warn = { fg = "#a22", bg = "#ddd" }
theme.error = { fg = "#222", bg = "#a22" }

return theme

-- vim: et:sw=4:ts=8:sts=4:tw=80
