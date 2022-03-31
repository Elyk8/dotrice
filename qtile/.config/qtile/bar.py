from libqtile import widget
from libqtile.bar import Bar
from unicodes import left_half_circle, right_half_circle

from colors import dark_plus as cl

bar = Bar([
    widget.GroupBox(
        font="Ubuntu Bold",
        fontsize=18,
        # margin_y=3,
        # margin_x=0,
        # padding_y=5,
        # padding_x=3,
        borderwidth=3,
        active=cl['fg'],
        inactive=cl['black'],
        rounded=False,
        highlight_color=cl['bg'],
        highlight_method="line",
        this_current_screen_border=cl['fg'],
        this_screen_border=cl['black'],
        other_current_screen_border=cl['fg'],
        other_screen_border=cl['black'],
        foreground=cl['fg'],
        background=cl['bg'],
    ),
    widget.WindowCount(),
    widget.CurrentLayout(),
    widget.Prompt(),
    widget.WindowName(),
    widget.Chord(
        chords_colors={
            "launch": ("#ff0000", "#ffffff"),
        },
        name_transform=lambda name: name.upper(),
    ),
    widget.Systray(
        padding=2,
        icon_size=24
    ),
    widget.BatteryIcon(),
    widget.Clock(format="%Y-%m-%d %a %I:%M %p"),
],
    background=cl['bg'],
    size=32,
    # border_width=[2, 0, 2, 0],  # Draw top and bottom borders
    # border_color=["ff00ff", "000000", "ff00ff", "000000"]  # Borders are magenta
)
