#!/usr/bin/env bash

echo "==> $USER"

echo "  -> Synchronizing music playlists"
~/.dotfiles/music-sync.sh

echo "  -> Updating neovim plugins"
nvim --headless -c 'autocmd User PackerComplete quitall' -c PackerSync
