#!/usr/bin/env bash

set -eu

cd "${0%/*}/defaults/"
config=${XDG_CONFIG_HOME:-$HOME/.config}/dotfiles/
[[ ! -f "$config/update.sh" ]] && install -Dm755 update.sh -t "$config"
[[ ! -f "$config/sway-config" ]] && install -Dm644 sway-config -t "$config"
