# http://forums.steampowered.com/forums/showthread.php?t=1268621
umount /.0000

cd /; sudo rm users

cd ~; rm library
cd ~/Library; rm application\ support
cd ~/Library/Application\ Support; rm steam

cd ~; rm documents
cd ~/Documents; rm steam\ content

open -W /.0000/Steam.app
