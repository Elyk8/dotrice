#!/usr/bin/env sh
tmuxconf="$HOME/.config/tmux"
selected="$(cat "$tmuxconf"/tmux-cht-languages "$tmuxconf"/tmux-cht-command | fzf)"
if [ -z "$selected" ]; then
	exit 0
fi

printf "Enter Query: "
read -r query

if grep -qs "$selected" "$tmuxconf"/tmux-cht-languages; then
	query=$(echo "$query" | tr ' ' '+')
	tmux neww bash -c "echo \"curl cht.sh/$selected/$query/\" & curl -s cht.sh/$selected/$query | less"
else
	tmux neww bash -c "curl -s cht.sh/$selected~$query | less"
fi
