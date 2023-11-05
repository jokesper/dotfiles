#!/usr/bin/env bash

set -eu

cd
mkdir -p \
	Documents \

cd "${0%/*}/user/"
config=${XDG_CONFIG_HOME:-$HOME/.config}/dotfiles/
[[ -d ~/.mozilla ]] && ln -rsf firefox.js "$(find  ~/.mozilla/firefox/ -name '*.default-release')/user.js"
[[ ! -f "$config/sway-config" ]] && install -Dm644 sway-config -t "$config"

cargo install cargo-update rust-script

hoogle generate

chsh -s "/usr/bin/fish"
