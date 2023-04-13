#!/usr/bin/env bash

set -eu

fallback='#0F0F0F'
[[ ! -v 1 ]] &&
	WPA_ID=$(wpa_cli status | awk -F= '/^id=/ {print $2; exit}')
case ${2:-${WPA_ID:+CONNECTED}} in
	CONNECTED)
		ssid=$(wpa_cli get_network "$WPA_ID" ssid \
			| tail -n 1 \
			| jq --raw-output .)
		swaymsg "output * background ~/Desktop/background-$ssid fit $fallback"
		;;
	*)
		swaymsg "output * background $fallback solid_color"
		;;
esac
