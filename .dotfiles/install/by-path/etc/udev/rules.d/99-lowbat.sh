#!/usr/bin/env bash

set -eu

case "${POWER_SUPPLY_STATUS:-Unknown}" in
	Discharging)
		[[ -a /tmp/warned-low-battery ]] && exit
		touch /tmp/warned-low-battery
		;;
	*)
		[[ -a /tmp/warned-low-battery ]] \
			&& rm /tmp/warned-low-battery
		exit
		;;
esac

for dbus in /run/user/*/bus; do
	uid=${dbus%/*}
	uid=${uid##*/}
	uname=$(id -un -- $uid)
	DBUS_SESSION_BUS_ADDRESS="unix:path=$dbus" \
		runuser \
		--whitelist-environment=DBUS_SESSION_BUS_ADDRESS \
		-u "$uname" \
		-- notify-send \
		--app-name=System \
		--urgency=critical \
		'Battery low' \
		"Battery at ${POWER_SUPPLY_CAPACITY:-??}%"
done
