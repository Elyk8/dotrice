#!/bin/sh

emacs --daemon --eval "(exwm-enable)"
exec dbus-launch --exit-with-session emacsclient -c
