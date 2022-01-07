#!/usr/bin/env bash

function run {
    if ! pgrep $1 > /dev/null ;
    then
        $@&
    fi
}

# run picom --experimental-backends &
# run nitrogen --restore &
run xsetroot -cursor_name left_ptr &
# run xfce4-power-manager &
# run xss-lock ~/.config/i3lock/lock.sh &
# run ~/.config/conky/hybrid-compact/startup/hybrid-compact-startup.sh &
# run ~/.config/conky/hybrid-compact1/startup/hybrid-compact-startup1.sh &
# run /usr/bin/startupsound &
run flashfocus &
# run lxsession &
# run dunst &
# run udiskie &
# run nm-applet &
# run pamac-tray &
# run volumeicon &
# run mpd &

# pactl set-default-sink alsa_output.pci-0000_00_1b.0.analog-stereo &

# run trayer --edge top --align right --widthtype request --padding 3 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x282a33 --monitor 1 --height 30 --iconspacing 4 &

# xrandr --output eDP1 --mode 1920x1080 --pos 3840x0 --rotate normal --output HDMI1 --mode 1920x1080 --pos 1920x0 --rotate normal --output VGA1 --primary --mode 1920x1080 --pos 0x0 --rotate normal --output VIRTUAL1 --off
