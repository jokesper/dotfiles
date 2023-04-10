#!/usr/bin/env bash

path=${0%/*}
cd "$path/install/"
install -Dm644 ./user-updates.hook -t /etc/pacman.d/hooks/
install -Dm755 ./user-updates.sh -t /opt/dotfiles/
install -Dm644 ./doas.conf -t /etc/
install -Dm644 ./faillock.conf -t /etc/security/
install -Dm644 ./personal.map -t /usr/local/share/kbd/keymaps/
install -Dm644 ./personal-xkb -T /usr/share/X11/xkb/symbols/personal
install -Dm644 ./vconsole.conf -t /etc/
