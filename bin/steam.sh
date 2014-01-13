# http://forums.steampowered.com/forums/showthread.php?t=1268621
hdiutil attach -mountpoint /.0000 /Users/dz0004455/Desktop/no_backup/Steam.sparseimage

ln -s /.0000/Steam ~/Library/Application\ Support/
ln -s /.0000/Steam\ Content ~/Documents/

cd /; sudo ln -s Users users

cd ~; ln -s Library library
cd ~/library; ln -s Application\ Support application\ support
cd ~/library/application\ support; ln -s Steam steam

cd ~; ln -s Documents documents
cd ~/documents; ln -s Steam\ Content steam\ content

#open -W /.0000/Steam.app

#umount /.0000

#cd /; sudo rm users
#
#cd ~; rm library
#cd ~/Library; rm application\ support
#cd ~/Library/Application\ Support; rm steam
#
#cd ~; rm documents
#cd ~/Documents; rm steam\ content
