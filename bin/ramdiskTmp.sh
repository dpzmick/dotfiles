# ram://size
# number of mb's desired * 2048
# number of gigs desired * 1024*2048
diskutil erasevolume HFS+ "tmp" `hdiutil attach -nomount ram://4194304`
ln -s /Volumes/tmp ~/tmp
