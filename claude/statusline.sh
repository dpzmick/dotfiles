#!/usr/bin/env bash
# Claude Code status line: hostname · git branch · working dir.
# Receives the session JSON on stdin; the first stdout line becomes the bar.
set -u

input=$(cat)

# Claude passes the active dir as workspace.current_dir (falls back to cwd).
dir=$(printf '%s' "$input" | jq -r '.workspace.current_dir // .cwd // empty' 2>/dev/null)
[ -n "$dir" ] || dir=$PWD

# Branch via read-only git (no index-lock churn on slow storage); short SHA if detached.
branch=$(GIT_OPTIONAL_LOCKS=0 git -C "$dir" symbolic-ref --short HEAD 2>/dev/null) \
  || branch=$(GIT_OPTIONAL_LOCKS=0 git -C "$dir" rev-parse --short HEAD 2>/dev/null) \
  || branch=""

host=$(hostname -s)
pretty=${dir/#$HOME/\~}              # abbreviate $HOME to ~

c_host=$'\e[36m'                     # cyan
c_branch=$'\e[32m'                   # green (matches the zsh prompt glyph)
c_dir=$'\e[33m'                      # yellow
gray=$'\e[90m'; reset=$'\e[0m'
sep=" ${gray}·${reset} "

out="${c_host}${host}${reset}"
[ -n "$branch" ] && out+="${sep}${c_branch} ${branch}${reset}"
out+="${sep}${c_dir}${pretty}${reset}"
printf '%s' "$out"
