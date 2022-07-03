#################################### PLUGINS ###################################
set -g @mode_indicator_prefix_prompt " #[fg=black]WAIT #[default]#[fg=green]"
set -g @mode_indicator_prefix_mode_style bg=green,fg=default
set -g @mode_indicator_copy_prompt " #[fg=black]COPY #[default]#[fg=cyan]"
set -g @mode_indicator_copy_mode_style bg=cyan,fg=default
set -g @mode_indicator_sync_prompt " #[fg=black]SYNC #[default]#[fg=red]"
set -g @mode_indicator_sync_mode_style bg=red,fg=default
set -g @mode_indicator_empty_prompt " #[fg=black]TMUX #[default]#[fg=blue]"
set -g @mode_indicator_empty_mode_style bg=blue,fg=default

# tmux cpu
set -g @cpu_percentage_format "%3.0f%%"

# tmux-online-status
# set -g @route_to_ping "ns1.telstra.net"
# set -g @online_icon ""
# set -g @offline_icon "#[fg=$color_red]"

# tmux-pomodoro
# set -g @pomodoro_on " #[fg=default] "
# set -g @pomodoro_complete " #[fg=green] "

# tmux-battery
# set -g @batt_icon_charge_tier8 ""
# set -g @batt_icon_charge_tier7 ""
# set -g @batt_icon_charge_tier6 ""
# set -g @batt_icon_charge_tier5 ""
# set -g @batt_icon_charge_tier4 ""
# set -g @batt_icon_charge_tier3 ""
# set -g @batt_icon_charge_tier2 ""
# set -g @batt_icon_charge_tier1 ""
#
# set -g @batt_icon_status_charged " "
# set -g @batt_icon_status_charging "  "
# set -g @batt_icon_status_discharging " "
# set -g @batt_icon_status_attached " "
# set -g @batt_icon_status_unknown " "

# set -g @batt_remain_short true

#################################### OPTIONS ###################################
set -g status on
set -g status-justify centre
set -g status-position bottom
set -g status-left-length 90
set -g status-right-length 90
set -g status-style "bg=default"
set -g window-style ""
set -g window-active-style ""

set -g message-style bg=red,fg=black
set -g status-left-style none
setw -g window-status-style bg=default,fg=black,none
setw -g window-status-current-style bg=default,fg=black
setw -g window-status-activity-style fg=green,none
setw -g window-status-separator ""
set-window-option -g mode-style bg=colour08,fg=default

#################################### FORMAT ####################################
# padding above status line
# setw -g pane-border-status bottom
# setw -g pane-border-format "─"
# setw -g pane-border-style fg=$color_gray
# setw -g pane-active-border-style fg=$color_gray

# set -g status-left "#{tmux_mode_indicator} #[fg=$color_gray]#{online_status}  %R"
set -g status-left "#{tmux_mode_indicator} %R"
# set -g status-right "#[fg=$color_gray]#{battery_icon_charge}  #{battery_percentage}#{battery_icon_status}#{battery_remain} | CPU:#{cpu_percentage}#{pomodoro_status}"
set -g status-right "#[fg=blue] CPU:#{cpu_percentage}"
setw -g window-status-format "#[fg=colour08,nobold,nounderscore,noitalics] #[fg=colour08] #I #W #[fg=colour08,nobold,nounderscore,noitalics]"
setw -g window-status-current-format "#[fg=colour08,bold,nounderscore,noitalics] #[fg=purple] #I #W #[fg=colour08,nobold,nounderscore,noitalics]"
