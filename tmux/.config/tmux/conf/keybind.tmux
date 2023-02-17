################################# KEY BINDINGS #################################
# Set a less awkward prefix
unbind C-b
set-option -g prefix C-a
bind C-a send-prefix

# Reload the tmux file
bind r source-file "$tmux_dir/tmux.conf"    # Displays "Tmux reloaded!"

# Bring back clear screen under tmux prefix
#bind-key C-l send-keys 'C-l'

###################################### VIM #####################################

# Tmux doesn't pass <S-CR> codes to Neovim
# https://stackoverflow.com/questions/16359878/how-to-map-shift-enter
bind -n S-Enter send-keys Escape "[13;2u"

# Easier exit
bind -n M-q \
  if-shell \
    '[ "$(tmux display-message -p "#{window_panes}")" -gt 1 ]' \
    'kill-pane; select-layout; select-layout -E' \
    'detach'

bind-key -r k copy-mode
bind -T copy-mode-vi v send -X begin-selection
bind -T copy-mode-vi y send-keys -X copy-pipe-and-cancel "pbcopy"
bind -T copy-mode-vi MouseDragEnd1Pane send-keys -X copy-pipe-and-cancel "pbcopy"
bind -T copy-mode-vi Enter send-keys -X copy-pipe-and-cancel "pbcopy"

## Custom functions
bind-key -r f run-shell "tmux neww tmux-sessionizer"
bind-key -r i run-shell "tmux neww tmux-cht.sh"
bind-key -r p run-shell "tmux neww pomo"
bind-key -r t run-shell "tmux neww typioca"
bind-key -r s run-shell "tmux neww \"fd -Hatf --base-directory $SCRIPTS | fzf | xargs -r $EDITOR\""
bind-key -r c run-shell "tmux neww \"fd -Hatf --base-directory $DOTS | fzf | xargs -r $EDITOR\""
bind-key -r n run-shell "tmux neww newsboat"
bind-key -r l run-shell "tmux neww lf #{pane_current_path}"
