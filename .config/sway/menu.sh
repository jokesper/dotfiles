#!/usr/bin/env bash

set -eu

find \
	-L $(sed 's/:/\/applications /g' <(printf "%q" \
		"${XDG_DATA_HOME:-$HOME/.local/share}:${XDG_DATA_DIRS:-/usr/local/share:/usr/share}:")) \
	-type f -name \*.desktop \
	| xargs -d '\n' gawk -f "${0%/*}/parse-desktop-entry.awk" \
	| sort \
	| gawk -F "\t" -v menu='bemenu -ipλ -fm all --fn "NimbusSans 8"' '
		{
			print $1 |& menu
			cmds[$1] = $2
		}

		END {
			close(menu, "to")
			menu |& getline name
			print cmds[name]
		}'
