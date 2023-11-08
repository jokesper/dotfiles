#!/usr/bin/env bash

set -eu

config=${XDG_CONFIG_HOME:-$HOME/.config}/dotfiles
data=${XDG_DATA_HOME:-$HOME/.local/share}/dotfiles

cd
mkdir -p \
	Documents \
	"$data"

cd "${0%/*}/user/"
[[ ! -f "$config/sway-config" ]] && install -Dm644 sway-config -t "$config"

cargo install cargo-update rust-script

[[ ! -d "$data/user.js.git" ]] \
	&& git clone --bare -- \
		'https://github.com/arkenfox/user.js.git' \
		"$data/user.js.git"

hoogle generate

chsh -s "/usr/bin/fish"
