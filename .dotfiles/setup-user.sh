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

for repo in {'rxi/json.lua','arkenfox/user.js','typst/typst'}; do
	local="$data/${repo#*/}.git"
	[[ ! -d "$local" ]] \
		&& git clone --depth=1 --bare -- \
			"https://github.com/$repo.git" \
			"$local"
done


hoogleDB=${XDG_DATA_HOME:-$HOME/.local/share}/hoogle/haskell.hoo
[[ ! -d "$hoogleDB" ]] && hoogle generate --database="$hoogleDB"

[[ "$(getent passwd "$USER" | cut -d: -f7)" != "$(which fish)" ]] \
	&& chsh -s "$(which fish)"
