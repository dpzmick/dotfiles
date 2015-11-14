#!/bin/sh

# set a random background
set-wallpaper.sh

# set capslock to ctrl
setxkbmap -option ctrl:nocaps

# start some tray apps
/opt/extras.ubuntu.com/touchpad-indicator/bin/touchpad-indicator &
nm-applet &
gtk-redshit &
trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --transparent true --alpha 0 --tint 0x000000 --width 5 --height 19 &
