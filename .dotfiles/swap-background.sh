#!/usr/bin/env bash

fallback='#0F0F0F'
if [ -z $1 ]; then
	WPA_ID=$(wpa_cli status | awk -F= '/^id=/ {print $2; exit}')
fi
case ${2:-$([[ -n $WPA_ID ]] && echo "CONNECTED")} in
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
