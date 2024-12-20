#!/usr/bin/env bash

set -eu

data=${XDG_DATA_HOME:-$HOME/.local/share}/dotfiles
state=${XDG_STATE_HOME:-$HOME/.local/state}/dotfiles
aur=$state/aur

lv1="\e[32m==>\e[0m"
lv2="\e[34m  ->\e[0m"
printf "$lv1 $USER\n"

# TODO:
# figure out how to handle parallel output

printf "$lv2 Synchronizing music playlists\n"
~/.dotfiles/music-sync.sh

printf "$lv2 Updating neovim plugins\n"
nvim --headless '+Lazy! sync' '+silent MasonUpdate' +quitall &

printf "$lv2 Fetching changes for local git repos\n"
for gitDir in {"$data"/*.git,"$aur"/*/.git}; do
	git --git-dir="$gitDir" fetch --depth=1 --quiet
done

printf "$lv2 Updating firefox user.js\n"
~/.dotfiles/merge-firefox-config.sh &

printf "$lv2 Updating fcitx5 quick phrases\n"
~/.dotfiles/typst-to-fcitx5.sh &

printf "$lv2 Updating haskell package index\n"
cabal update

printf "$lv2 Updating local haskell binaries\n"
for bin in "$HOME"/.dotfiles/*/*.cabal; do
	(cd "${bin%/*}"; cabal install --ghc-options=-dynamic --overwrite-policy=always)
done

printf "$lv2 Updating AUR packages\n"
for pkg in "$aur"/*/.git; do
	diff=$(git --git-dir="$pkg" diff --color=always HEAD...origin/master)
	if [[ -n "$diff" ]]; then
		printf '%s\n' "$diff" | less -R
		dir=${pkg%/*}
		name=${dir##*/}
		read -rp "Update '$name'? [Y/n] "
		if [[ $REPLY =~ ^[Yy]?$ ]]; then (
			cd "$dir"
			git merge --ff-only --quiet origin/master
			makepkg --syncdeps --rmdeps --install --force
		) fi
	fi
done

printf "$lv2 Waiting for tasks to finish\n"
wait
