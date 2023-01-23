#!/bin/sh

# for sudo to respect aliases
alias sudo="sudo "
# pip aliases
alias pip-upgrade="pip freeze --user | cut -d'=' -f1 | xargs -n1 pip install -U"
alias pip-upgrade-venv="pip freeze | cut -d'=' -f1 | xargs -n1 pip install -U"

#fix obvious typo's
alias cd..='cd ..'
alias pdw="pwd"
alias udpate='sudo pacman -Syyu'
alias upate='sudo pacman -Syyu'
alias updte='sudo pacman -Syyu'
alias updqte='sudo pacman -Syyu'
alias upqll="paru -Syu --noconfirm"
alias upal="paru -Syu --noconfirm"

#better cd
alias ...='cd ../..'
alias ....='cd ../../..'
alias .....='cd ../../../..'

# z.lua useful aliases
alias j='z -I'  # use fzf to select in multiple matches
alias jj='z -c' # restrict matches to subdirs of $PWD
alias zz='z -c' # restrict matches to subdirs of $PWD
alias zi='z -i' # cd with interactive selection
alias zb='z -b' # quickly cd to the parent directory

# readable output
alias df='df -h'
alias free="free -mth"

# pacman unlock
alias unlock="sudo rm /var/lib/pacman/db.lck"
alias rmpacmanlock="sudo rm /var/lib/pacman/db.lck"

# continue download
alias wget="wget -c --hsts-file='\$XDG_CACHE_HOME/wget-hsts'"

# userlist
alias userlist="cut -d: -f1 /etc/passwd"

# merge new settings
alias merge="xrdb -merge ~/.Xresources"

# Aliases for software managment
# pacman or pm
alias pac='sudo pacman -S'
alias pm='sudo pacman'
alias pmr='sudo pacman -Rns'
alias update='sudo pacman -Syyu'

# Fzf
alias fpac='pacman -Slq | fzf --multi --preview "pacman -Si {1}" | xargs -ro sudo pacman -S'
alias fpm='pacman -Slq | fzf --multi --preview "pacman -Si {1}" | xargs -ro sudo pacman -S'
alias fpr='paru -Slq | fzf --multi --preview "paru -Si {1}" | xargs -ro paru -S'
alias fpmr='pacman -Qq | fzf --multi --preview "pacman -Qi {1}" | xargs -ro sudo pacman -Rns'

# paru as aur helper - updates everything
alias pksyua="paru -Syu --noconfirm"
alias upall="paru -Syu --noconfirm"

# ps
alias psa="ps auxf"
alias psgrep="ps aux | grep -v grep | grep -i -e VSZ -e"

# grub update
alias update-grub="sudo grub-mkconfig -o /boot/grub/grub.cfg"

# add new fonts
alias update-fc='sudo fc-cache -fv'

# hardware info --short
alias hw="hwinfo --short"

# skip integrity check
alias paruskip='paru -S --mflags --skipinteg'

# check vulnerabilities microcode
alias microcode='grep . /sys/devices/system/cpu/vulnerabilities/*'

# get fastest mirrors in your neighborhood
alias mirror="sudo reflector -f 30 -l 30 --number 10 --verbose --save /etc/pacman.d/mirrorlist-arch"
alias mirrord="sudo reflector --latest 30 --number 10 --sort delay --save /etc/pacman.d/mirrorlist-arch"
alias mirrors="sudo reflector --latest 30 --number 10 --sort score --save /etc/pacman.d/mirrorlist-arch"
alias mirrora="sudo reflector --latest 30 --number 10 --sort age --save /etc/pacman.d/mirrorlist-arch"
# our experimental - best option for the moment
alias mirrorx="sudo reflector --age 6 --latest 20  --fastest 20 --threads 5 --sort rate --protocol https --save /etc/pacman.d/mirrorlist-arch"
alias mirrorxx="sudo reflector --age 6 --latest 20  --fastest 20 --threads 20 --sort rate --protocol https --save /etc/pacman.d/mirrorlist-arch"
alias ram='rate-mirrors --allow-root arch | sudo tee /etc/pacman.d/mirrorlist-arch'

# Youtube downloader
alias yta-aac="yt-dlp --embed-metadata --extract-audio --audio-format aac "
alias yta-best="yt-dlp --embed-metadata --extract-audio --audio-format best "
alias yta-flac="yt-dlp --embed-metadata --extract-audio --audio-format flac "
alias yta-mp3="yt-dlp --embed-metadata --extract-audio --audio-format mp3 "
alias ytv-best="yt-dlp --embed-metadata -f 'bestvideo[ext=mp4]+bestaudio[ext=m4a]/bestvideo+bestaudio' --merge-output-format mp4 "

# Recent Installed Packages
alias rip="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -200 | nl"
alias riplong="expac --timefmt='%Y-%m-%d %T' '%l\t%n %v' | sort | tail -3000 | nl"

#Cleanup orphaned packages
alias cleanup='sudo pacman -Rns $(pacman -Qtdq)'

#search content with ripgrep
alias rg="rg --sort path"

#get the error messages from journalctl
alias jctl="journalctl -p 3 -xb"

# edit for important configuration files know what you do in these files
alias npacman="sudoedit /etc/pacman.conf"
alias ngrub="sudoedit /etc/default/grub"
alias nconfgrub="sudoedit /boot/grub/grub.cfg"
alias nmkinitcpio="sudoedit /etc/mkinitcpio.conf"
alias nmirrorlist="sudoedit /etc/pacman.d/mirrorlist"
alias narcomirrorlist='sudoedit /etc/pacman.d/arcolinux-mirrorlist'
alias nfstab="sudoedit /etc/fstab"
alias nnsswitch="sudoedit /etc/nsswitch.conf"
alias nsamba="sudoedit /etc/samba/smb.conf"
alias ngnupgconf="sudoedit /etc/pacman.d/gnupg/gpg.conf"

#gpg
#verify signature for isos
alias gpg-check="gpg2 --keyserver-options auto-key-retrieve --verify"
alias fix-gpg-check="gpg2 --keyserver-options auto-key-retrieve --verify"
#receive the key of a developer
alias gpg-retrieve="gpg2 --keyserver-options auto-key-retrieve --receive-keys"
alias fix-gpg-retrieve="gpg2 --keyserver-options auto-key-retrieve --receive-keys"
alias fix-keyserver="[ -d ~/.gnupg ] || mkdir ~/.gnupg ; cp /etc/pacman.d/gnupg/gpg.conf ~/.gnupg/ ; echo 'done'"

#fixes
alias fix-permissions="sudo chown -R \$USER:\$USER ~/.config ~/.local"

#maintenance
alias big="expac -H M '%m\t%n' | sort -h | nl"
alias downgrada="sudo downgrade --ala-url https://ant.seedhost.eu/arcolinux/"

#systeminfo
alias probe="sudo -E hw-probe -all -upload"
alias sysfailed="systemctl list-units --failed"

#remove
alias rmgitcache="rm -r ~/.cache/git"

# Verbosity and settings that you pretty much just always are going to want.
alias cp="cp -ivr"
alias trash="trash -v"
alias mv="mv -iv"
alias rm="rm -Ivr"
alias bc="bc -ql"
alias mkdir="mkdir -pv"
alias ln="ln -v"

#Git
alias gc='git clone'
alias gs='git status'
alias ga='git add'
alias gco='git commit -a'
alias gp='git push'
alias gpl='git pull'
alias lg='lazygit'

# Colorize commands when possible.
alias l='exa --group-directories-first --git'
alias ll='exa --icons --all -l --group-directories-first --git'
alias ls='exa --icons --group-directories-first --git'
alias lt='exa -T --git-ignore --level=2 --group-directories-first'
alias llt='exa -lT --git-ignore --level=2 --group-directories-first'
alias lT='exa -T --git-ignore --level=4 --group-directories-first'
alias l.="exa --all | rg '^\.'"
alias tree="exa --tree -F"
alias grep="grep --color=auto"
alias egrep='egrep --color=auto'
alias fgrep='fgrep --color=auto'
alias diff="diff --color=auto"

# Others
alias stow="stow --target \$HOME"
alias ka="killall"
alias re='source $ZDOTDIR/.zshrc'
alias sumake='sudo make clean install'
alias his='history'
alias py='python'
alias dun='du -sh *'
alias duh='du -d 1 -h'
alias du='du -h'
alias lin='exa | wc -lines'
alias lih='exa -a | wc -lines'
alias wion='nmcli radio wifi on'
alias wiof='nmcli radio wifi off'
alias xp='chmod +x'
alias ffmpeg="ffmpeg -hide_banner"
alias e="\$EDITOR"
alias br='bulk-rename'
alias removemeta="exiftool -recurse -overwrite_original -all= --icc_profile:all"

se() { fd -atf --base-directory "$XDG_CONFIG_HOME" | fzf | xargs -r "$EDITOR"; }
sr() { fd -atf --base-directory "$SCRIPTS" | fzf | xargs -r "$EDITOR"; }
sc() { fd -Hatf --base-directory "$DOTS" | fzf | xargs -r "$EDITOR"; }
xevv() { xev | awk -F'[ )]+' '/^KeyPress/ { a[NR+2] } NR in a { printf "%-3s %s\n", $5, $8 }'; }

riceros() {
	conda activate ros
	export ROS_MASTER_URI=http://10.1.1.100:11311
	export ROS_HOSTNAME=10.1.1.100
	export TURTLEBOT3_MODEL=burger # waffle, waffle_pi
	# alias teleop="roslaunch turtlebot3_teleop turtlebot3_teleop_key.launch"
	# alias slam="roslaunch turtlebot3_slam turtlebot3_slam.launch"
}

lfcd() {
	tmp="$(mktemp)"
	lf -last-dir-path="$tmp" "$@"
	if [ -f "$tmp" ]; then
		dir="$(cat "$tmp")"
		\rm -f "$tmp"
		if [ -d "$dir" ]; then
			if [ "$dir" != "$(pwd)" ]; then
				cd "$dir"
			fi
		fi
	fi
}

alias lf="lfcd"
