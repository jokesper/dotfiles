#!/usr/bin/env bash

set -eu

printf ":: Running user update scripts...\n"
getent passwd \
	| gawk -F: \
		$(gawk '/^UID_(MIN|MAX)/ {print "-v "$1"="$2}' /etc/login.defs) \
		'{if (UID_MIN <= $3 && $3 <= UID_MAX) print $1}' \
	| doas xargs -rI% runuser -u % -- \
		bash -c '${XDG_CONFIG_HOME:-$HOME/.config}/dotfiles/update.sh'
