#!/usr/bin/env bash

set -eu

# toggle-window-action.sh <condition> <command-true> <workspace> <command-false> [<hook-for-false>]
# Example:
#	Play/pause cmus or if cmus isn't open, open it on workspace 10 and start playing
#	`toggle-window-action.sh 'pgrep cmus' 'cmus-remote --pause' \$ws10 'kitty -d ~/Music cmus' 'cmus-remote --play'`

if $1; then $2; else
	ws=$(swaymsg -t get_workspaces \
		| jq -r '.[]
			| select(.focused == true)
			| .name')
	swaymsg -- workspace --no-auto-back-and-forth "$3"
	swaymsg -- "exec $4"
	[[ -v 5 ]] && timeout 3s bash -c "until $5; do sleep 0.1; done"
	swaymsg -- workspace --no-auto-back-and-forth "$ws"
fi
