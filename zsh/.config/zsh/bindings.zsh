# Key-bindings
bindkey -s '^n' '$EDITOR $(fzf)^M'
bindkey -s '^v' '$EDITOR\n'
bindkey -s '^z' 'zi^M'
bindkey -s '^w' '!!^M'
bindkey '^[[P' delete-char
bindkey "^p" up-line-or-beginning-search # Up
bindkey "^n" down-line-or-beginning-search # Down
bindkey '^k' autosuggest-accept
bindkey '^h' backward-delete-word
bindkey -r "^u"
bindkey -r "^d"
bindkey -s '^x' '^usource $ZDOTDIR/.zshrc\n'

# Expand on Ctrl-Space
my-expand() { zle _expand_alias || zle .expand-word || true; }
zle -N my-expand
bindkey '^e' my-expand
