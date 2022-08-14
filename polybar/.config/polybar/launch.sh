#!/usr/bin/env sh

if ps -C herbstluftwm >/dev/null 2>&1; then
  LAUNCH="herbst-bar"
elif ps -C i3 >/dev/null 2>&1; then
  LAUNCH="i3-bar"
fi


# Terminate already running bar instances
killall -q polybar

# Wait until the processes have been shut down
while pgrep -x polybar >/dev/null; do sleep 1; done

screens=$(xrandr --listactivemonitors | grep -v "Monitors" | cut -d" " -f6)

if [ "$(xrandr --listactivemonitors | awk '/Monitors/ {print $2}' )" = 1 ]; then
  MONITOR=$(polybar --list-monitors | cut -d":" -f1) polybar "${LAUNCH}1" &
else
  primary=$(xrandr --query | grep primary | cut -d" " -f1)

  for m in $screens; do
    if [ "$primary" = "$m" ]; then
        MONITOR=$m TRAY_POS=right polybar "${LAUNCH}1" &
    else
        MONITOR=$m TRAY_POS=none polybar "${LAUNCH}2" &
    fi
  done
fi
