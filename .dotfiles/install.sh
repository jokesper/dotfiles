#!/usr/bin/env bash

path=${0%/*}
cd "$path/install/"
install -Dm644 ./doas.conf -t /etc/
install -Dm644 ./faillock.conf -t /etc/security/
install -Dm644 ./25-wireless.network -t /etc/systemd/network/
install -Dm644 ./personal.map -t /usr/local/share/kbd/keymaps/
install -Dm644 ./personal-xkb -T /usr/share/X11/xkb/symbols/personal
install -Dm644 ./vconsole.conf -t /etc/

while IFS= read -r device; do
	config="/etc/wpa_supplicant/wpa_supplicant-$device.conf"
	[[ ! -f $config ]] && install -Dm600 ./wpa_supplicant.conf -T $config
	systemctl enable "wpa_supplicant@$device"
done <<< "$(networkctl list --json=short \
	| jq -r '.Interfaces[]
		| select(.Type == "wlan")
		| .Name')"

systemctl enable \
	systemd-networkd \
	systemd-resolved \
	systemd-timesyncd \
