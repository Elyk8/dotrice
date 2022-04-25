#!/bin/sh

killall -q polybar

# Wait until the processes have been shut down
while pgrep -u $UID -x polybar > /dev/null; do sleep 1; done

echo "---" | tee -a /tmp/polybar1.log /tmp/polybar2.log
polybar i3monitor1 2>&1 | tee -a /tmp/polybar1.log & disown
polybar i3monitor2 2>&1 | tee -a /tmp/polybar2.log & disown

echo "Bars launched..."
