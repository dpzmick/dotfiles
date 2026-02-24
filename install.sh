#!/usr/bin/env bash
#
# bootstrap installs things.

# shamelessly stolen from
# https://github.com/holman/dotfiles/blob/master/script/bootstrap

DOTFILES_ROOT=$(pwd -P)

set -e

echo ''

info () {
  printf "\r  [ \033[00;34m..\033[0m ] $1\n"
}

user () {
  printf "\r  [ \033[0;33m??\033[0m ] $1\n"
}

success () {
  printf "\r\033[2K  [ \033[00;32mOK\033[0m ] $1\n"
}

fail () {
  printf "\r\033[2K  [\033[0;31mFAIL\033[0m] $1\n"
  echo ''
  exit
}

setup_gitconfig () {
  if ! [ -f git/gitconfig.symlink ]
  then
    info 'setup gitconfig'

    git_credential='cache'
    if [ "$(uname -s)" == "Darwin" ]
    then
      git_credential='osxkeychain'
    fi

    user ' - What is your github author name?'
    read -e git_authorname
    user ' - What is your github author email?'
    read -e git_authoremail

    sed -e "s/AUTHORNAME/$git_authorname/g" -e "s/AUTHOREMAIL/$git_authoremail/g" -e "s/GIT_CREDENTIAL_HELPER/$git_credential/g" git/gitconfig.symlink.example > git/gitconfig.symlink

    success 'gitconfig'
  fi
}

link_file () {
  local src=$1 dst=$2

  local overwrite= backup= skip=
  local action=

  if [ -f "$dst" -o -d "$dst" -o -L "$dst" ]
  then

    if [ "$overwrite_all" == "false" ] && [ "$backup_all" == "false" ] && [ "$skip_all" == "false" ]
    then

      local currentSrc="$(readlink $dst)"

      if [ "$currentSrc" == "$src" ]
      then

        skip=true;

      else

        user "File already exists: $dst ($(basename "$src")), what do you want to do?\n\
        [s]kip, [S]kip all, [o]verwrite, [O]verwrite all, [b]ackup, [B]ackup all?"
        read -n 1 action

        case "$action" in
          o )
            overwrite=true;;
          O )
            overwrite_all=true;;
          b )
            backup=true;;
          B )
            backup_all=true;;
          s )
            skip=true;;
          S )
            skip_all=true;;
          * )
            ;;
        esac

      fi

    fi

    overwrite=${overwrite:-$overwrite_all}
    backup=${backup:-$backup_all}
    skip=${skip:-$skip_all}

    if [ "$overwrite" == "true" ]
    then
      rm -rf "$dst"
      success "removed $dst"
    fi

    if [ "$backup" == "true" ]
    then
      mv "$dst" "${dst}.backup"
      success "moved $dst to ${dst}.backup"
    fi

    if [ "$skip" == "true" ]
    then
      success "skipped $src"
    fi
  fi

  if [ "$skip" != "true" ]  # "false" or empty
  then
    ln -s "$1" "$2"
    success "linked $1 to $2"
  fi
}

install_dotfiles () {
  info 'installing dotfiles'

  local overwrite_all=false backup_all=false skip_all=false

  for src in $(find -H "$DOTFILES_ROOT" -maxdepth 2 -name '*.symlink' -not -path '*.git*' -not -name 'config.symlink')
  do
    dst="$HOME/.$(basename "${src%.*}")"
    link_file "$src" "$dst"
  done
}

install_config () {
  info 'installing config symlinks'

  local overwrite_all=false backup_all=false skip_all=false

  mkdir -p "$HOME/.config"

  for src in "$DOTFILES_ROOT"/config.symlink/*/
  do
    [ -d "$src" ] || continue
    src="${src%/}"  # strip trailing slash
    local name="$(basename "$src")"
    local dst="$HOME/.config/$name"
    link_file "$src" "$dst"
  done
}

setup_submodules () {
  info 'initializing submodules'
  git -C "$DOTFILES_ROOT" submodule update --init --recursive
  success 'submodules ready'
}

select_profile () {
  local profile_file="$DOTFILES_ROOT/.profile"

  if [ -f "$profile_file" ]; then
    PROFILE=$(cat "$profile_file")
    info "using profile: $PROFILE"
    return
  fi

  local profiles=(generic arch)

  echo ''
  user 'Select a profile for this machine:'
  for i in "${!profiles[@]}"; do
    printf "    %d) %s\n" $((i+1)) "${profiles[$i]}"
  done
  printf "  > "
  read -r choice

  if [ "$choice" -ge 1 ] 2>/dev/null && [ "$choice" -le "${#profiles[@]}" ] 2>/dev/null; then
    PROFILE="${profiles[$((choice-1))]}"
  else
    fail "invalid selection"
  fi

  echo "$PROFILE" > "$profile_file"
  success "profile set to $PROFILE"
}

install_profile () {
  local profile_config="$DOTFILES_ROOT/profiles/$PROFILE/config"

  if [ ! -d "$profile_config" ]; then
    info "no profile-specific config for '$PROFILE'"
    return
  fi

  info "installing profile config for '$PROFILE'"

  local overwrite_all=false backup_all=false skip_all=false

  mkdir -p "$HOME/.config"

  for src in "$profile_config"/*/; do
    [ -d "$src" ] || continue
    src="${src%/}"
    local name="$(basename "$src")"
    local dst="$HOME/.config/$name"
    link_file "$src" "$dst"
  done

  # symlink top-level files (e.g. clean-chroot-manager.conf)
  for src in "$profile_config"/*; do
    [ -f "$src" ] || continue
    local name="$(basename "$src")"
    local dst="$HOME/.config/$name"
    link_file "$src" "$dst"
  done
}

select_profile
setup_gitconfig
setup_submodules
install_dotfiles
install_config
install_profile

echo ''
echo '  All installed!'
