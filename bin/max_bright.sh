#!/bin/bash

maxbrightness=`cat /sys/class/backlight/intel_backlight/max_brightness`
brightness=/sys/class/backlight/intel_backlight/brightness

sudo tee $brightness <<< $maxbrightness
