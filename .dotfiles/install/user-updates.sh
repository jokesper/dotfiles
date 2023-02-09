#!/usr/bin/env bash

getent passwd \
	| gawk -F: \
		-v run='xargs -0 runuser -u' \
		$(gawk '/^UID_(MIN|MAX)/ {print "-v "$1"="$2}' /etc/login.defs) \
		'{
			if (UID_MIN <= $3 && $3 <= UID_MAX)
				print $1"\0--\0bash\0-c\0\"" \
					"${XDG_CONFIG_HOME:-$HOME/.config}/dotfiles/update.sh" \
					"\"\0" | run
		}'
