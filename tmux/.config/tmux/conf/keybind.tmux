################################# KEY BINDINGS #################################
# Set a less awkward prefix
unbind C-b
set-option -g prefix C-a
bind C-a send-prefix

# Reload the tmux file
bind r source-file "$tmux_dir/tmux.conf"    # Displays "Tmux reloaded!"

# Bring back clear screen under tmux prefix
bind-key C-l send-keys 'C-l'

# Windows
bind w kill-window
bind -n S-Left  previous-window
bind -n S-Right next-window

# Panes
bind s split-window -v               # Horizontal
bind v split-window -h               # Vertical
bind k kill-pane -a                  # Kill all panes except for the current one

###################################### VIM #####################################

# Tmux doesn't pass <S-CR> codes to Neovim
# https://stackoverflow.com/questions/16359878/how-to-map-shift-enter
bind -n S-Enter send-keys Escape "[13;2u"

# Smart pane switching with awareness of Vim splits.
# From https://github.com/christoomey/vim-tmux-navigator
is_vim="ps -o state= -o comm= -t '#{pane_tty}' \
| grep -iqE '^[^TXZ ]+ +(\\S+\\/)?g?(view|n?vim?x?)(diff)?$'"

# check if the pane is a fzf/fzy instance
is_fzf="ps -o state= -o comm= -t '#{pane_tty}' \
  | grep -iqE '^[^TXZ ]+ +(\\S+\\/)?fz[f,y]$'"

# movement using the Control key
bind -n C-h run "($is_vim && tmux send-keys C-h) || \
                          tmux select-pane -L"

bind -n C-j run "($is_vim && tmux send-keys C-j)  || \
                         ($is_fzf && tmux send-keys C-j) || \
                         tmux select-pane -D"

bind -n C-k run "($is_vim && tmux send-keys C-k) || \
                          ($is_fzf && tmux send-keys C-k)  || \
                          tmux select-pane -U"

bind -n C-l run  "($is_vim && tmux send-keys C-l) || \
                          tmux select-pane -R"

bind-key -T copy-mode-vi 'C-h' if -F '#{pane_at_left}' '' 'select-pane -L'
bind-key -T copy-mode-vi 'C-j' if -F '#{pane_at_bottom}' '' 'select-pane -D'
bind-key -T copy-mode-vi 'C-k' if -F '#{pane_at_top}' '' 'select-pane -U'
bind-key -T copy-mode-vi 'C-l' if -F '#{pane_at_right}' '' 'select-pane -R'

# resize using the Alt key
bind -n 'M-h' if-shell "$is_vim" 'send-keys M-h' 'resize-pane -L 5'
bind -n 'M-j' if-shell "$is_vim" 'send-keys M-j' 'resize-pane -D 5'
bind -n 'M-k' if-shell "$is_vim" 'send-keys M-k' 'resize-pane -U 5'
bind -n 'M-l' if-shell "$is_vim" 'send-keys M-l' 'resize-pane -R 5'

bind-key -T copy-mode-vi M-h resize-pane -L 1
bind-key -T copy-mode-vi M-j resize-pane -D 1
bind-key -T copy-mode-vi M-k resize-pane -U 1
bind-key -T copy-mode-vi M-l resize-pane -R 1
