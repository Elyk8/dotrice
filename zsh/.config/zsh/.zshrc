# Zap
[ -f "$HOME/.local/share/zap/zap.zsh" ] && source "$HOME/.local/share/zap/zap.zsh"

# Main
plug "$ZDOTDIR/options.zsh"
plug "$ZDOTDIR/bindings.zsh"


# Plugins
#zapplug "zap-zsh/supercharge"
plug "zap-zsh/fzf"
plug "zsh-users/zsh-autosuggestions"
plug "zsh-users/zsh-syntax-highlighting"
plug "hlissner/zsh-autopair"
plug "zap-zsh/vim"
plug "Aloxaf/fzf-tab"
plug "skywind3000/z.lua"

# Theme
plug "zap-zsh/zap-prompt"

# Completion
plug "esc/conda-zsh-completion"

# Other options
plug "$ZDOTDIR/chpwd.zsh"
plug "$ZDOTDIR/aliases.zsh"

# Tmux
# tmx main > /dev/null 2>&1
