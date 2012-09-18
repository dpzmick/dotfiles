# ram://size
# number of mb's desired * 2048
# number of gigs desired * 1024*2048
diskutil erasevolume HFS+ "tmp" `hdiutil attach -nomount ram://4194304`
ln -s /Volumes/tmp ~/tmp

# set up browser caches

# chromium
# tries to keep cache data the same to avoid upsetting the browser
mkdir ~/tmp/cache/
mkdir ~/tmp/cache/chrome
mv ~/Library/Caches/Chromium/Default/Cache ~/tmp/cache/chrome/Cache
rm -rf ~/Library/Caches/Chromium/Default
ln -s ~/tmp/cache/chrome ~/Library/Caches/Chromium/Default
