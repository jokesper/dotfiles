#!/usr/bin/env bash

set -eu

cd
mkdir -p \
	Documents \

cd "${0%/*}/user/"
config=${XDG_CONFIG_HOME:-$HOME/.config}/dotfiles/
[[ ! -f "$config/sway-config" ]] && install -Dm644 sway-config -t "$config"

cargo install cargo-update rust-script

hoogle generate

chsh -s "/usr/bin/fish"
