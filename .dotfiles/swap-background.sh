#!/usr/bin/env bash

#set -eu

set-wallpaper() {
	wallpaper=$(find "$2" -type f 2>/dev/null | shuf -n1)
	[[ -z "$wallpaper" ]] && return 1
	swaymsg output "$1" background "$wallpaper" fit "#7F0000"
}

[[ ! -v 1 ]] && WPA_ID=$(wpa_cli status | awk -F= '/^id=/ {print $2; exit}')
[[ (! -v 2 || $2 == 'CONNECTED') && -n "$WPA_ID" ]] &&
	ssid=$(wpa_cli get_network "$WPA_ID" ssid \
		| tail -n 1 \
		| jq --raw-output .)

swaymsg -t get_outputs \
| jq -r '.[].name' \
| while IFS= read -r output; do
	set-wallpaper "$output" "$HOME/Desktop/background-${ssid:-offline}=$output" ||
	set-wallpaper "$output" "$HOME/Desktop/background-${ssid:-offline}" ||
	set-wallpaper "$output" "$HOME/Desktop/background=$output" ||
	set-wallpaper "$output" "$HOME/Desktop/background" ||
	swaymsg output "$output" background "#0F0F0F" solid_color
done
