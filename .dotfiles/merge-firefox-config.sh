#!/usr/bin/env bash

set -eu

data=${XDG_DATA_HOME:-$HOME/.local/share}/dotfiles
profile=~/.mozilla/firefox/dotfiles

git --git-dir="$data/user.js.git" show HEAD:user.js \
	| sed -z \
		-e 's!/\*\([^*]\|\*[^/]\)*\*/!!g' \
		-e 's!\([\n;]\)[[:space:]]*//[^\n]*!\1!g' \
		-e 's/\n\+/\n/g' \
	| cat - "$profile/user-override.js" \
	> "$profile/user.js"
