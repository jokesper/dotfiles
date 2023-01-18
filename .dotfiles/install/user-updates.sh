#!/usr/bin/env bash

getent passwd \
	| gawk -F: \
		-v run='xargs -d: runuser -u' \
		$(gawk '/^UID_(MIN|MAX)/ {print "-v "$1"="$2}' /etc/login.defs) \
		'{if (UID_MIN <= $3 && $3 <= UID_MAX) print $1":"$6"/.update:" | run}'
