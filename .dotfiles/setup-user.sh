#!/usr/bin/env bash

set -eu

config=${XDG_CONFIG_HOME:-$HOME/.config}/dotfiles
data=${XDG_DATA_HOME:-$HOME/.local/share}/dotfiles
state=${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles
aur=$state/aur

cd
mkdir -p \
	Documents \
	"$data"

cd "${0%/*}/user/"
[[ ! -f "$config/hyprland-config" ]] && install -Dm644 hyprland-config -t "$config"

cargo install cargo-update

[[ ! -d "$data/json.lua.git" ]] \
	&& git clone --bare -- \
		'https://github.com/rxi/json.lua.git' \
		"$data/json.lua.git"
[[ ! -d "$data/user.js.git" ]] \
	&& git clone --bare -- \
		'https://github.com/arkenfox/user.js.git' \
		"$data/user.js.git"

for pkg in {kmonad-bin,swww}; do
	[[ ! -d "$aur/$pkg" ]] \
		&& git clone -- \
			"https://aur.archlinux.org/$pkg.git" \
			"$aur/$pkg"
done

[[ ! -d "$HOME/.hoogle" ]] && hoogle generate

[[ "$(getent passwd "$USER" | cut -d: -f7)" != "$(which fish)" ]] \
	&& chsh -s "$(which fish)"

~/.dotfiles/user-updates.sh
