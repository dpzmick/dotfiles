# fix browser cache
# otherwise I think they get really upset
rm ~/Library/Caches/Chromium/Default
mkdir ~/Library/Caches/Chromium/Default/
mv ~/tmp/cache/chrome/Cache ~/Library/Caches/Chromium/Default/

# remove disk
hdiutil detach -force ~/tmp
rm ~/tmp
