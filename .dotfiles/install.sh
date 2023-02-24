#!/usr/bin/env bash

path=${0%/*}
cd "$path/install/"
install -Dm644 ./user-updates.hook -t /etc/pacman.d/hooks/
install -Dm755 ./user-updates.sh -t /opt/dotfiles/
install -Dm644 ./doas.conf -t /etc/doas.conf

user=${DOAS_USER:-$SUDO_USER}
if [ -z $user ]; then
	printf "Not running from \`doas\` or \`sudo\`"
	user=$(stat -c %U "$path")
	printf "Detected home directory of %s" "$user"
fi
runuser -u "$user" ./../setup-user.sh
