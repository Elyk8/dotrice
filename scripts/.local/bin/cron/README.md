# NOTE

Some cronjobs have components that require information about your current
display to display notifications correctly.

Put the following snippet of commands in the scripts to ensure that
notifications properly.

```bash
environs=`pidof dbus-daemon | tr ' ' '\n' | awk '{printf "/proc/%s/environ ", $1}'`
export DBUS_SESSION_BUS_ADDRESS=`cat $environs 2>/dev/null | tr '\0' '\n' | grep DBUS_SESSION_BUS_ADDRESS | cut -d '=' -f2-`
export DISPLAY=:0
```

This will also make sure that environment variables and xdotool commands
function properly.
