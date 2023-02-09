#!/usr/bin/env bash

cd "${0%/*}/defaults/"
config=${XDG_CONFIG_HOME:-$HOME/.config}/dotfiles/
install -Dm755 ./update.sh -t "$config"
install -Dm644 ./sway-config -t "$config"
