#!/usr/bin/env bash

set -eu

before() {
	add-package \
		wl-clipboard \
		xdg-desktop-portal-hyprland \
		`#hyprpolkitagent` \
		hyprlock \
		hypridle \
		swaybg \
		waybar \
		wofi \
		grim slurp \
		cpio \
		cmake \

}
