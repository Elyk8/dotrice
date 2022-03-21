#!/bin/sh
colorTrayer=$(xrdb -query | awk '/trayerColor/ {print $2}')
h=32
killall trayer
sleep 1
trayer -l --edge bottom --align right --widthtype request --padding 10 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint "$colorTrayer" --monitor "primary" --height "$h" --iconspacing 0
