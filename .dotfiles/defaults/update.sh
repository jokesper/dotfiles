#!/usr/bin/env bash

set -eu

lv1="\e[32m==>\e[0m"
lv2="\e[34m  ->\e[0m"
printf "$lv1 $USER\n"

printf "$lv2 Synchronizing music playlists\n"
~/.dotfiles/music-sync.sh

printf "$lv2 Updating neovim plugins\n"
nvim --headless -c 'autocmd User PackerComplete quitall' -c PackerSync
