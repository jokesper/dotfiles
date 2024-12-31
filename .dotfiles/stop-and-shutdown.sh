#!/usr/bin/env bash

set -eu

error() { printf "\e[31;1m%s: %s\e[0m\n" "$0" "$*" >&2; }

if ! cmus-remote -Q &>/dev/null; then poweroff; fi

if [[ ! -f /tmp/stop-and-shutdown.lock ]] then (
	touch /tmp/stop-and-shutdown.lock
	state=$(cmus-remote -Q)
	status=$(sed -ne 's/^status \([a-z]\+\)/\1/p' <<< "$state")
	continue=$(sed -ne 's/^set continue \(true\|false\)/\1/p' <<< "$state")
	duration=$(sed -ne 's/^duration \([0-9]\+\)/\1/p' <<< "$state")
	position=$(sed -ne 's/^position \([0-9]\+\)/\1/p' <<< "$state")

	if [[ "$status" == "playing" ]] then
		remaining=$(( $duration - $position ))
		cmus-remote -C 'set continue=0'
		sleep "$remaining"
		cmus-remote -C "set continue=$continue"
		sleep .5
	fi

	cmus-remote -C quit
	rm /tmp/stop-and-shutdown.lock

	sleep .5

	poweroff
) & disown
else
	error 'Already running'
	exit 1
fi
