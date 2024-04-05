#!/usr/bin/env bash

set -eu

after() {
	[[ ! -f /etc/wpa_supplicant/wpa_supplicant.conf ]] \
		&& install -Dm600 ./wpa_supplicant.conf -t /etc/wpa_supplicant/

	while IFS= read -r device; do
		[[ -z $device ]] && continue
		config="/etc/wpa_supplicant/wpa_supplicant-$device.conf"
		[[ ! -f $config ]] && install -Dm600 /etc/wpa_supplicant/wpa_supplicant.conf -T "$config"
		systemctl enable "wpa_supplicant@$device"
	done <<< "$(networkctl list --json=short \
		| jq -r '.Interfaces[]
			| select(.Type == "wlan")
			| .Name')"
}
