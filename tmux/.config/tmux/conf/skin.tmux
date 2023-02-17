# Use a monochrome statusline, with host and session names on the right.
# set-option -g status-position top
# set -g status-bg black
# set -g status-fg white
# set -g status-left ''
# set -g status-right ' #h: #S '

# # Use normal and bright monochrome colors to show a tab line on the left.
# set -g status-justify left
# set-window-option -g window-status-separator ''
# set-window-option -g window-status-format '#[bg=black,fg=white] #I #W '
# set-window-option -g window-status-current-format '#[bg=brightblack,fg=brightwhite] #I #W '

# # Switching panes should change the foreground color but not border colors.
# set -g window-style 'bg=default,fg=white'
# set -g window-active-style 'bg=default,fg=brightwhite'
# set -g pane-border-style 'bg=default,fg=black'
# set -g pane-active-border-style 'bg=default,fg=white'


set -g status-position top               # statusbar position
set -g status-interval 1
set -g window-status-format '#I:#(pwd="#{pane_current_path}"; echo ${pwd####*/})'
set -g window-status-current-format '[#I:#(pwd="#{pane_current_path}"; echo ${pwd####*/})]'
set -g status-right-length 120
set -g status-right '#(date +"%b %_d %H:%M") | #(whoami)@#(hostname -s) '
# set -g window-status-current-attr bold
