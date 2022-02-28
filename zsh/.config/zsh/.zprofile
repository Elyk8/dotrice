# Scaling
export QT_QPA_PLATFORMTHEME="qt5ct"
export QT_STYLE_OVERRIDE=kvantum
export PLASMA_USE_QT_SCALING=1
#export QT_AUTO_SCREEN_SCALE_FACTOR=0
#export QT_SCALE_FACTOR=1
#export QT_SCREEN_SCALE_FACTORS="1;1;1"
#export GDK_SCALE=1
#export GDK_DPI_SCALE=1

# export DESKTOP_SESSION=dusk
# export XDG_SESSION_TYPE=x11

# if pacman -Qs libxft-bgra >/dev/null 2>&1; then
	if [ -z "${DISPLAY}" ] && [ "$(tty)" = "/dev/tty1" ]; then
		pidof -s Xorg &> /dev/null && exit 0
		sudo /usr/bin/prime-switch &> /dev/null
		exec startx "${XDG_CONFIG_HOME:-$HOME/.config}/x11/xinitrc" &> /dev/null
	fi
# else
# 	echo "\033[31mIMPORTANT\033[0m: Note that \033[32m\`libxft-bgra\`\033[0m must be installed for this build of dusk.
# Please run:
# 	\033[32mparu -S libxft-bgra-git\033[0m
# and replace \`libxft\`. Afterwards, you may start the graphical server by running \`startx\`."
# fi
