#!/usr/bin/env bash

path=${0%/*}
cd "$path/install/"
install -Dm644 ./user-updates.hook -t /etc/pacman.d/hooks/
install -Dm755 ./user-updates.sh -t /opt/dotfiles/
install -Dm644 ./doas.conf -t /etc/
install -Dm644 ./faillock.conf -t /etc/security/
