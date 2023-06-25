#!/usr/bin/env bash

set -eu

lv1="\e[32m==>\e[0m"
lv2="\e[34m  ->\e[0m"
printf "$lv1 $USER\n"

printf "$lv2 Synchronizing music playlists\n"
~/.dotfiles/music-sync.sh

printf "$lv2 Updating neovim plugins\n"
nvim --headless -c 'autocmd User PackerComplete quitall' -c PackerSync 2>/dev/null

printf "$lv2 Updating rust toolchain\n"
rustup update

printf "$lv2 Updating cargo packages\n"
printf "Not working due to bashrc not running to set environment"
#cargo install-update --all
