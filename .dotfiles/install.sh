#!/usr/bin/env bash

cd "${0%/*}/install/"
install -Dm644 ./user-updates.hook -t /etc/pacman.d/hooks/
install -Dm755 ./user-updates.sh -t /opt/dotfiles/
