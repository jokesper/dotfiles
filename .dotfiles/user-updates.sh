#!/usr/bin/env bash

set -eu

data=${XDG_DATA_HOME:-$HOME/.local/share}/dotfiles

lv1="\e[32m==>\e[0m"
lv2="\e[34m  ->\e[0m"
printf "$lv1 $USER\n"

printf "$lv2 Synchronizing music playlists\n"
~/.dotfiles/music-sync.sh

printf "$lv2 Updating neovim plugins\n"
nvim --headless '+Lazy! sync' +MasonUpdate +quitall 2>/dev/null

printf "$lv2 Fetching changes for local git repos\n"
for gitDir in "$data"/*.git; do git --git-dir="$gitDir" fetch --quiet; done

printf "$lv2 Updating firefox user.js\n"
~/.dotfiles/merge-firefox-config.sh

printf "$lv2 Updating cargo packages\n"
cargo install-update --quiet --all

printf "$lv2 Updating cabal\n"
cabal update
