#!/usr/bin/env bash

set -eu

before() {
	add-package \
		wl-clipboard \
		xdg-desktop-portal-hyprland \
		`#polkit-kde-agent` \
		hyprlock \
		hypridle \
		waybar \
		wofi \
		grim slurp \
		swww \

}
