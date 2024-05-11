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

for repo in {'rxi/json.lua','arkenfox/user.js'}; do
	local="$data/${repo#*/}.git"
	[[ ! -d "$local" ]] \
		&& git clone --filter=blob:none --bare -- \
			"https://github.com/$repo.git" \
			"$local"
done

for pkg in {kmonad-bin,swww}; do
	if [[ ! -d "$aur/$pkg" ]]; then
		git clone --filter=blob:none -- \
			"https://aur.archlinux.org/$pkg.git" \
			"$aur/$pkg"
		cd "$aur/$pkg"
		makepkg --syncdeps --rmdeps --install --needed
	fi
done

[[ ! -d "$HOME/.hoogle" ]] && hoogle generate

[[ "$(getent passwd "$USER" | cut -d: -f7)" != "$(which fish)" ]] \
	&& chsh -s "$(which fish)"
