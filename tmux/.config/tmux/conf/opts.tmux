# utf8 is on
setw -q -g utf8 on

# Fix colors and enable true color support and italics
# set -g default-terminal "tmux-256color"
# tell Tmux that outside terminal supports true color
set -ag terminal-overrides ",xterm-256color:RGB"

# Use the mouse
set -g mouse on

# Sticky time after repeated commands
set -sg repeat-time 500

# Start numbering at 1 for windows and panes
set -g base-index 1
setw -g pane-base-index 1

# Automatically rename windows based on the application within
setw -g automatic-rename on

# Renumber windows if others are closed
set -g renumber-windows on

# Use titles in tabs
set -g set-titles on

# Aggresively resize
setw -g aggressive-resize on
