# Zap
[ -f "$HOME/.local/share/zap/zap.zsh" ] && source "$HOME/.local/share/zap/zap.zsh"

function zsh_add_file() {
    zapsource "$ZDOTDIR/$1"
}

# Main
_try_source "$ZDOTDIR/options.zsh"
_try_source "$ZDOTDIR/bindings.zsh"


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
plug "esc/conda-zsh-completion" false

# Other options
_try_source "$ZDOTDIR/chpwd.zsh"
_try_source "$ZDOTDIR/aliases.zsh"

# Tmux
# tmx main > /dev/null 2>&1
