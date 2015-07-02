# dotfiles

# Install
require ruby and rake, if you don't have these you will have to add symlinks
manually

TODO eventually remove dependency on ruby there is no reason for this

1. clone this repo somewhere, like ~/dotfiles
2. rake install
3. (n)vim +PlugInstall +qall

WARNING: By default, this also installs some git stuff, you might start
committing as David Zmick after installing these. Be careful!!

# Features
The only thing here that is really that interesting is the vim config, the rest
is just here as a convenience to me.

## Vim
This is by no means a complete list, but here are some highlights:

* syntax checking via syntastic (or youcompleteme)
* indication of changes in version control (vim-signify)
* snippets via ulitsnips
* undo that persists between editing sessions
* Super cool status bar, reports things like syntastic errors, if there is
  trailing whitespace in the file anywhere, git branch
* unite.vim for all sorts of things
* jk for escape all over the place
* delimitmate for auto insert of delimiters
* easyalign (if I can figure out how to use it)
* a variety of plugins for a bunch of different languages

There are all sorts of other things in the vimrc, just go look at it

### Keybindings (for complete reference just look in vimrc)

* <Ctrl>P opens recursive fuzzy file search on files in current working directory
* <Ctrl>O opens search on currently opened buffers
* <Ctrl>T opens fuzzy search on tags in file
* <Leader>{k,j,h,l} moves between open windows (or dvtm panes)
* <Leader>C clears highlighted searches
* <Leader>W removes trailing whitespace

leader is set to ,

## zsh

* Hit enter for ls! TODO fix color difference between "ls" and "l"
* Some things still broken here. Switching between insert and normal mode
  doesn't always update indicator and sometimes moves the prompt up a line.
  (there is a zsh bug for this somewher)
