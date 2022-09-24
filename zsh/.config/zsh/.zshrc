#!/bin/sh
HISTFILE=~/.cache/zsh-history
setopt appendhistory

# some useful options (man zshoptions)
setopt autocd extendedglob nomatch menucomplete
setopt interactive_comments
setopt aliases
stty stop undef # Disable ctrl-s to freeze terminal.
zle_highlight=('paste:none')

# beeping is annoying
unsetopt BEEP

# History in cache directory:
HISTSIZE=10000000
SAVEHIST=10000000

# completions
autoload -Uz compinit
autoload -Uz bashcompinit
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|[._-]=* r:|=*' 'l:|=* r:|=*'
# zstyle ':completion::complete:lsof:*' menu yes select
zmodload zsh/complist
for dump in $ZDOTDIR/.zcompdump(N.mh+24); do
  compinit
  bashcompinit
done
compinit -C
bashcompinit -C
_comp_options+=(globdots)       # Include hidden files.

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

# Colors
autoload -Uz colors && colors

# Useful Functions
source "$ZDOTDIR/zsh-functions"
zsh_add_file "lfcd"

# Plugins
zsh_add_plugin "zsh-users/zsh-autosuggestions"
zsh_add_plugin "zsh-users/zsh-syntax-highlighting"
zsh_add_plugin "hlissner/zsh-autopair"
zsh_add_plugin "skywind3000/z.lua"
zsh_add_completion "esc/conda-zsh-completion" false
# For more plugins: https://github.com/unixorn/awesome-zsh-plugins
# More completions https://github.com/zsh-users/zsh-completions

# Shortcuts
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/aliasrc"
[ -f "${XDG_CONFIG_HOME:-$HOME/.config}/shell/apparix" ] && source "${XDG_CONFIG_HOME:-$HOME/.config}/shell/apparix"

# FZF
[ -f /usr/share/fzf/completion.zsh ] && source /usr/share/fzf/completion.zsh
[ -f /usr/share/fzf/key-bindings.zsh ] && source /usr/share/fzf/key-bindings.zsh
[ -f /usr/share/doc/fzf/examples/completion.zsh ] && source /usr/share/doc/fzf/examples/completion.zsh
[ -f /usr/share/doc/fzf/examples/key-bindings.zsh ] && source /usr/share/doc/fzf/examples/key-bindings.zsh
[ -f /usr/share/clifm/functions/file_picker.sh ] && source /usr/share/clifm/functions/file_picker.sh
[ -f /usr/share/clifm/functions/cd_on_quit.sh ] && source /usr/share/clifm/functions/cd_on_quit.sh
[ -f $ZDOTDIR/completion/_fnm ] && fpath+="$ZDOTDIR/completion/"
# export FZF_DEFAULT_COMMAND='rg --hidden -l ""'

# Tmux
tmx main > /dev/null 2>&1

# Edit line in vim with ctrl-e:
autoload edit-command-line; zle -N edit-command-line >/dev/null
ZSH_AUTOSUGGEST_HIGHLIGHT_STYLE="fg=#4c4c4c,bold,underline"

# Normal files to source
zsh_add_file "zsh-vim-mode"
zsh_add_file "zsh-chpwd"
zsh_add_file "zsh-keys"
# zsh_add_file "zsh-prompt"

eval "$(starship init zsh)"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/home/elyk/.local/share/mambaforge/bin/conda' 'shell.zsh' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/home/elyk/.local/share/mambaforge/etc/profile.d/conda.sh" ]; then
        . "/home/elyk/.local/share/mambaforge/etc/profile.d/conda.sh"
    else
        export PATH="/home/elyk/.local/share/mambaforge/bin:$PATH"
    fi
fi
unset __conda_setup

if [ -f "/home/elyk/.local/share/mambaforge/etc/profile.d/mamba.sh" ]; then
    . "/home/elyk/.local/share/mambaforge/etc/profile.d/mamba.sh"
fi
# <<< conda initialize <<<

