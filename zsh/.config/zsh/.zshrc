# Zap
[ -f "$HOME/.local/share/zap/zap.zsh" ] && source "$HOME/.local/share/zap/zap.zsh"

function zsh_add_file() {
    zapsource "$ZDOTDIR/$1"
}

# Main
zsh_add_file "options.zsh"
zsh_add_file "bindings.zsh"


# Plugins
#zapplug "zap-zsh/supercharge"
zapplug "zsh-users/zsh-autosuggestions"
zapplug "zsh-users/zsh-syntax-highlighting"
zapplug "hlissner/zsh-autopair"
zapplug "zap-zsh/vim"
zapplug "Aloxaf/fzf-tab"
zapplug "skywind3000/z.lua"

# Theme
zapplug "zap-zsh/zap-prompt"

# Completion
zapcmp "esc/conda-zsh-completion" false

# Other options
zapsource "/usr/share/fzf/completion.zsh"
zapsource "/usr/share/fzf/key-bindings.zsh"
zapsource "/usr/share/doc/fzf/examples/completion.zsh"
zapsource "/usr/share/doc/fzf/examples/key-bindings.zsh"

zsh_add_file "chpwd.zsh"
zsh_add_file "aliases.zsh"

# Tmux
tmx main > /dev/null 2>&1
