from libqtile import layout
from libqtile.config import Click, Drag, Group, Match, Screen
from libqtile.config import ScratchPad, DropDown
from libqtile.config import Key, KeyChord
from libqtile.lazy import lazy

from bar import bar
from colors import dark_plus as cl

# Local variables
mod = "mod4"
terminal = "st"
emacs = "emacsclient -cn"

keys = [
    # A list of available commands that can be bound to keys can be found
    # at https://docs.qtile.org/en/latest/manual/config/lazy.html
    # LEADER BINDINGS
    KeyChord(
        [mod],
        "space",
        [
            Key([], "t", lazy.spawn(terminal), desc="Launch Terminal"),
            Key([], "e", lazy.spawn(emacs), desc="Launch Emacs"),
            Key([], "r", lazy.spawn([terminal + " -e lf"]), desc="Launch lf"),
            Key([], "w", lazy.spawn("firefox"), desc="Launch firefox"),
            Key([], "b", lazy.spawn("dmenu-win"), desc="Dmenu find windows"),
            Key([], "c", lazy.spawn("brave"), desc="Launch brave"),
            Key([], "space", lazy.spawn("dm-j4dmenu-desktop"), desc="Dmenu launcher"),
            Key([], "m", lazy.spawn("mic-toggle"), desc="Mute/Unmute microphone"),
            KeyChord(
                [],
                "o",
                [
                    Key([], "d", lazy.spawn("discord"), desc="Launch discord"),
                    Key([], "f", lazy.spawn("/media/FTBA/FTBApp"), desc="Launch FTB Launcher"),
                ],
            ),
            KeyChord(
                [],
                "p",
                [
                    Key([], "a", lazy.spawn("dm-man")),
                    Key([], "c", lazy.spawn("clipmenu")),
                    Key([], "d", lazy.spawn("dm-directory")),
                    Key(["shift"], "c", lazy.spawn("dm-colorscheme")),
                    Key([], "e", lazy.spawn("dm-emoji")),
                    Key([], "k", lazy.spawn("dm-kill")),
                    Key([], "m", lazy.spawn("dm-buku")),
                    Key([], "o", lazy.spawn("dm-mount")),
                    Key([], "p", lazy.spawn("dm-passmenu")),
                    Key([], "b", lazy.spawn("dm-beats")),
                    Key([], "s", lazy.spawn("dm-scripts")),
                    Key([], "u", lazy.spawn("dm-umount")),
                    Key([], "w", lazy.spawn("weatherforecast")),
                ],
            ),
            KeyChord(
                [],
                ";",
                [
                    Key([], "a", lazy.spawn("setwallpaper a2n")),
                    Key([], "d", lazy.spawn("setwallpaper dt")),
                    Key([], "e", lazy.spawn("setwallpaper elyk")),
                    Key([], "v", lazy.spawn([terminal + " -e pulsemixer"])),
                ],
            ),
        ],
    ),
    Key([mod], "Escape", lazy.spawn("sysact"), desc="Power menu"),

>>>>>>> dc98d66 (commit)
    # Window controls
    Key([mod], "j", lazy.layout.down(), desc='Move focus down in current stack pane'),
    Key([mod], "k", lazy.layout.up(), desc='Move focus up in current stack pane'),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), lazy.layout.section_down(), desc='Move windows down in current stack'),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), lazy.layout.section_up(), desc='Move windows up in current stack'),
    Key([mod], "h", lazy.layout.shrink(), lazy.layout.decrease_nmaster(), desc='Shrink window (MonadTall), decrease number in master pane (Tile)'),
    Key([mod], "l", lazy.layout.grow(), lazy.layout.increase_nmaster(), desc='Expand window (MonadTall), increase number in master pane (Tile)'),
    Key([mod], "n", lazy.layout.normalize(), desc='normalize window size ratios'),
    Key([mod], "m", lazy.layout.maximize(), desc='toggle window between minimum and maximum sizes'),

    # Move windows between left/right columns or move up/down in current stack.
    # Moving out of range in Columns layout will create new column.
    Key([mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"),
    Key([mod, "shift"], "l", lazy.layout.shuffle_right(), desc="Move window to the right",),
    Key(
        [mod, "shift"], "h", lazy.layout.shuffle_left(), desc="Move window to the left"
    ),
    Key(
        [mod, "shift"],
        "l",
        lazy.layout.shuffle_right(),
        desc="Move window to the right",
    ),
    Key([mod, "shift"], "j", lazy.layout.shuffle_down(), desc="Move window down"),
    Key([mod, "shift"], "k", lazy.layout.shuffle_up(), desc="Move window up"),
    # Grow windows. If current window is on the edge of screen and direction
    # will be to screen edge - window would shrink.
    Key([mod, "control"], "h", lazy.layout.grow_left(),
        desc="Grow window to the left"),
    Key(
        [mod, "control"], "l", lazy.layout.grow_right(), desc="Grow window to the right"
    ),
    Key([mod, "control"], "j", lazy.layout.grow_down(), desc="Grow window down"),
    Key([mod, "control"], "k", lazy.layout.grow_up(), desc="Grow window up"),
    Key([mod], "n", lazy.layout.normalize(), desc="Reset all window sizes"),
    # Toggle between split and unsplit sides of stack.
    # Split = all windows displayed
    # Unsplit = 1 window displayed, like Max layout, but still with
    # multiple stack panes
    Key([mod, "shift"], "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
        ),
    Key(
        [mod, "shift"],
        "Return",
        lazy.layout.toggle_split(),
        desc="Toggle between split and unsplit sides of stack",
    ),
>>>>>>> dc98d66 (commit)

    # Toggle between different layouts as defined below
    Key([mod], "b", lazy.next_layout(), desc="Toggle between layouts"),
    Key([mod], "q", lazy.window.kill(), desc="Kill focused window"),
    Key([mod, "control"], "r", lazy.reload_config(), desc="Reload the config"),
    Key([mod, "control"], "q", lazy.shutdown(), desc="Shutdown Qtile"),

    # Toggle fullscreen and floating
    Key([mod], "f", lazy.window.toggle_fullscreen()),
    Key([mod], "t", lazy.window.toggle_floating()),

    # Multi monitor
    Key([mod], "w", lazy.to_screen(1)),
    Key([mod], "e", lazy.to_screen(0)),
]

#groups = [Group(i) for i in "123456789"]
groups = [
    Group("1", label="一"),
    Group("2", label="二"),
    Group("3", label="三"),
    Group("4", label="四"),
    Group("5", label="五"),
    Group("6", label="六"),
    Group("7", label="七"),
    Group("8", label="八"),
    Group("9", label="九"),
    Group("0", label="十"),
]

pinned_groups = ['12345', '67890']
all_groups = ''.join(pinned_groups)

for j, names in enumerate(pinned_groups):
    keys.extend(Key([mod], i, lazy.to_screen(j), lazy.group[i].toscreen()) for i in names)

keys.extend(Key([mod, 'shift'], i, lazy.window.togroup(i)) for i in all_groups)

groups.append(ScratchPad("scratchpad", [
    DropDown("mixer", [terminal + " -e pulsemixer"], width=0.4, on_focus_lost_hide=True),
    DropDown("ncmpcpp", [terminal + " -e ncmpcpp"], opacity=0.9, width=0.8, height=0.9, x=0.1, y=0.05, on_focus_lost_hide=True),
]))
]

for i in groups:
    keys.extend(
        [
            # mod1 + letter of group = switch to group
            Key(
                [mod],
                i.name,
                lazy.group[i.name].toscreen(),
                desc="Switch to group {}".format(i.name),
            ),
            # mod1 + shift + letter of group = switch to & move focused window to group
            Key(
                [mod, "shift"],
                i.name,
                lazy.window.togroup(i.name, switch_group=True),
                desc="Switch to & move focused window to group {}".format(
                    i.name),
            ),
            # Or, use below if you prefer not to switch to that group.
            # # mod1 + shift + letter of group = move focused window to group
            # Key([mod, "shift"], i.name, lazy.window.togroup(i.name),
            #     desc="move focused window to group {}".format(i.name)),
        ]
    )
>>>>>>> dc98d66 (commit)


layoutTheme = {"border_width": 3,
               "margin": 15,
               "border_focus": cl['blue'],
               "border_normal": cl['black']
               }

layouts = [
    # layout.Stack(num_stacks=2),
    # layout.Bsp(),
    # layout.Matrix(),
    layout.MonadTall(**layoutTheme),
    layout.MonadWide(**layoutTheme),
    layout.Max(**layoutTheme),
    # layout.RatioTile(),
    # layout.Tile(),
    # layout.TreeTab(),
    # layout.VerticalTile(),
    layout.Zoomy(**layoutTheme),
]

widget_defaults = dict(
    font="monospace",
    fontsize=16,
    padding=3,
)
extension_defaults = widget_defaults.copy()

screens = [Screen(bottom=bar)]

# Drag floating layouts.
mouse = [
    Drag(
        [mod],
        "Button1",
        lazy.window.set_position_floating(),
        start=lazy.window.get_position(),
    ),
    Drag(
        [mod], "Button3", lazy.window.set_size_floating(), start=lazy.window.get_size()
    ),
    Click([mod], "Button2", lazy.window.bring_to_front()),
]

dgroups_key_binder = None
dgroups_app_rules = []  # type: list
follow_mouse_focus = True
bring_front_click = False
cursor_warp = True
floating_layout = layout.Floating(
    float_rules=[
        # Run the utility of `xprop` to see the wm class and name of an X client.
        *layout.Floating.default_float_rules,
        Match(wm_class="confirmreset"),  # gitk
        Match(wm_class="makebranch"),  # gitk
        Match(wm_class="maketag"),  # gitk
        Match(wm_class="ssh-askpass"),  # ssh-askpass
        Match(title="branchdialog"),  # gitk
        Match(title="pinentry"),  # GPG key password entry
    ]
)
auto_fullscreen = False
focus_on_window_activation = "smart"
reconfigure_screens = True

# If things like steam games want to auto-minimize themselves when losing
# focus, should we respect this or not?
auto_minimize = True

# When using the Wayland backend, this can be used to configure input devices.
wl_input_rules = None

# XXX: Gasp! We're lying here. In fact, nobody really uses or cares about this
# string besides java UI toolkits; you can see several discussions on the
# mailing lists, GitHub issues, and other WM documentation that suggest setting
# this string if your java app doesn't work correctly. We may as well just lie
# and say that we're a working one by default.
#
# We choose LG3D to maximize irony: it is a 3D non-reparenting WM written in
# java that happens to be on java's whitelist.
wmname = "LG3D"
