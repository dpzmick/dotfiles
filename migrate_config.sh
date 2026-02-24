#!/usr/bin/env bash
#
# Migrate ~/.config from a symlink (pointing at config.symlink/) to a real
# directory. Only submodule entries are kept in config.symlink/; everything
# else is moved into the real ~/.config/.
#
# Safe to run multiple times — it's a no-op if already migrated.

set -e

DOTFILES_ROOT="$(cd "$(dirname "$0")" && pwd -P)"
CONFIG_SYMLINK="$DOTFILES_ROOT/config.symlink"
TARGET="$HOME/.config"

info () { printf "  [ .. ] %s\n" "$1"; }
success () { printf "  [ OK ] %s\n" "$1"; }

# Collect submodule paths (relative to repo root) that live under config.symlink/
submodule_children=()
while IFS= read -r path; do
  # path is relative to repo root, e.g. "config.symlink/base16-shell"
  name="${path#config.symlink/}"
  if [ -n "$name" ]; then
    submodule_children+=("$name")
  fi
done < <(git -C "$DOTFILES_ROOT" config --file .gitmodules --get-regexp '^submodule\..*\.path$' \
         | awk '{print $2}' \
         | grep '^config\.symlink/' || true)

is_submodule () {
  local name="$1"
  for sm in "${submodule_children[@]}"; do
    if [ "$sm" = "$name" ]; then
      return 0
    fi
  done
  return 1
}

# --- Pre-flight check ---
if [ ! -L "$TARGET" ]; then
  success "~/.config is already a real directory (or doesn't exist). Nothing to do."
  exit 0
fi

link_dest="$(readlink "$TARGET")"
if [ "$link_dest" != "$CONFIG_SYMLINK" ]; then
  echo "~/.config is a symlink but points to '$link_dest', not '$CONFIG_SYMLINK'."
  echo "Refusing to migrate — please check manually."
  exit 1
fi

info "~/.config is a symlink to config.symlink/. Migrating..."

# 1. Remove the symlink
rm "$TARGET"
success "removed ~/.config symlink"

# 2. Create real directory
mkdir -p "$TARGET"
success "created real ~/.config/"

# 3. Move non-submodule children from config.symlink/ into ~/.config/
for item in "$CONFIG_SYMLINK"/*; do
  [ -e "$item" ] || continue
  name="$(basename "$item")"

  # Skip dotfiles (like .gitignore)
  [[ "$name" == .* ]] && continue

  if is_submodule "$name"; then
    info "keeping submodule: $name"
  else
    mv "$item" "$TARGET/$name"
    success "moved $name to ~/.config/"
  fi
done

# 4. Remove .gitignore from config.symlink (no longer needed)
if [ -f "$CONFIG_SYMLINK/.gitignore" ]; then
  rm "$CONFIG_SYMLINK/.gitignore"
  success "removed config.symlink/.gitignore"
fi

echo ""
echo "Migration complete! Now run ./install.sh to create per-child symlinks."
