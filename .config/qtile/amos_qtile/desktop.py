"""Pure desktop declarations and explicitly subscribed taskbar callbacks."""

from libqtile import bar, layout, qtile, widget
from libqtile.config import Group, Match, MatchAny, Rule, ScratchPad, Screen

from . import declarations, patches


follow_mouse_focus = False
bring_front_click = "floating_only"
floats_kept_above = True
cursor_warp = False
auto_fullscreen = False
focus_on_window_activation = "urgent"
reconfigure_screens = True
auto_minimize = False
wmname = "LG3D"


def build_desktop(snapshot):
    scratchpad_matches = declarations.build_matches(snapshot)
    groups = [
        ScratchPad("scratchpad", declarations.build_dropdowns(snapshot)),
        Group("h"),
        Group("2", layout="max"),
        Group("c", layouts=[layout.Max()]),
    ]
    groups.extend(Group(name) for name in ["w", "e", "d", "f", "v", "n", "i", "o"])
    dgroups_app_rules = [
        Rule(Match(wm_class="kitty", title="local"), group="e"),
        Rule(Match(wm_class="kitty", title="work"), group="v"),
        Rule(Match(wm_class="kitty", title="remote"), group="i"),
        Rule(Match(wm_class="wemeetapp"), group="2"),
        Rule(Match(wm_class="xfreerdp"), group="w"),
        Rule(Match(wm_class="Google-chrome"), group="f"),
    ]
    layouts = [layout.MonadTall(border_width=0), layout.Max()]
    screens = [
        Screen(
            top=bar.Bar(
                [
                    widget.TaskList(
                        borderwidth=0,
                        font="Ubuntu Mono",
                        fontsize=32,
                        highlight_method="block",
                        padding_x=8,
                        padding_y=2,
                        rounded=False,
                    ),
                ],
                44,
            )
        )
    ]
    floating_border_colors = patches.ConditionalBorderColor(
        default="#FFB300",
        matches=[
            (scratchpad_matches["ioa"], "#F94144"),
            (scratchpad_matches["tdesktop"], "#70A288"),
            (scratchpad_matches["webchat"], "#2A9D8F"),
            (scratchpad_matches["chatgpt"], "#84A98C"),
            (scratchpad_matches["bookmarks"], "#4F6D7A"),
            (scratchpad_matches["stardict"], "#E83E8C"),
            (scratchpad_matches["stalonetray"], "#F2CC8F"),
            (Match(wm_class="Google-chrome", role="pop-up"), "#6A994E"),
            (Match(wm_class="copyq"), "#E9C46A"),
        ],
    )
    floating_layout = layout.Floating(
        border_width=patches.ConditionalBorderWidth(
            default=8,
            matches=[
                (Match(title="urxvt_scratchpad"), 0),
                (Match(wm_class="flameshot"), 0),
                (Match(wm_class="kitty", title="float"), 0),
                (Match(wm_class="kitty", title="dtpick"), 0),
            ],
        ),
        border_focus=floating_border_colors,
        border_normal=floating_border_colors,
        float_rules=[
            MatchAny(*layout.Floating.default_float_rules)
            & ~Match(wm_class="xfreerdp")
            & ~Match(wm_class="mpv"),
            Match(wm_class="Google-chrome", role="pop-up"),
            Match(wm_class="copyq"),
            Match(wm_class="TelegramDesktop"),
            Match(wm_class="kitty", title="dtpick"),
        ],
    )
    return {
        "groups": groups,
        "layouts": layouts,
        "screens": screens,
        "dgroups_app_rules": dgroups_app_rules,
        "floating_layout": floating_layout,
        "follow_mouse_focus": follow_mouse_focus,
        "bring_front_click": bring_front_click,
        "floats_kept_above": floats_kept_above,
        "cursor_warp": cursor_warp,
        "auto_fullscreen": auto_fullscreen,
        "focus_on_window_activation": focus_on_window_activation,
        "reconfigure_screens": reconfigure_screens,
        "auto_minimize": auto_minimize,
        "wmname": wmname,
    }


def show_browser_tabs(manager=None):
    manager = qtile if manager is None else manager
    for screen in manager.screens:
        if screen.top is not None:
            screen.top.show(screen.group.name == "c")


def initialize_browser_tabs(manager=None):
    show_browser_tabs(manager)
