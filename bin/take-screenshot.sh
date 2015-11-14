#!/bin/sh
scrot -s /tmp/screenshot.png
cat /tmp/screenshot.png | xclip -selection c -t image/png
rm /tmp/screenshot.png
