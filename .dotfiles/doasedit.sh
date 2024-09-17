#!/usr/bin/env bash

set -eu

tmp="/tmp/$(tr -dc A-Za-z0-9 </dev/urandom | head -c12)-${2##*/}"
mode=$(stat -c '%a' "$2")
own=$(stat -c '%u:%g' "$2")

cp "$2" "$tmp"
chmod 600 "$tmp"
chown "$DOAS_USER:" "$tmp"

SHELL="$(getent passwd joe | cut -d: -f7)" \
	runuser --pty -u "$DOAS_USER" -- $1 "$tmp"

mv "$tmp" "$2"
chmod $mode "$2"
chown $own "$2"
