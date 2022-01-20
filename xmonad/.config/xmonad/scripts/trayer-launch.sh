#!/bin/sh
colorTrayer=$(xrdb -query | awk '/trayerColor/ {print $2}')
killall trayer
sleep 1
trayer -l --edge top --align right --widthtype request --padding 5 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint "$colorTrayer" --monitor "primary" --height 30 --iconspacing 4
