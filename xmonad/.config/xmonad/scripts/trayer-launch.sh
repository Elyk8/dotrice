#!/bin/sh
colorTrayer=$(xrdb -get trayerColor)
h=32
killall trayer
sleep 1
trayer -l --edge top --align right --widthtype request --padding 2 --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint "$colorTrayer" --monitor "primary" --height "$h" --iconspacing 1
