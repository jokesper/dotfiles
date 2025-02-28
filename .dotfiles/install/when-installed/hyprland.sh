#!/usr/bin/env bash

set -eu

before() {
	add-package \
		wl-clipboard \
		xdg-desktop-portal-hyprland \
		`#hyprpolkitagent` \
		hyprlock \
		hypridle \
		waybar \
		wofi \
		grim slurp \
		swww \
		cpio \
		cmake \

}
