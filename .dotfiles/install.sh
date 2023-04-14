#!/usr/bin/env bash

set -eu

pacman --needed --noconfirm -S \
		gcc \
		jq \
		which \
	grub \
	pulseaudio \
	brightnessctl \
	man-db \
	doas \
	neovim \
		ripgrep \
		texlive-latexextra \
	sway \
		bemenu-wayland \
		grim slurp \
	kitty \
		ttf-fira-code \
	firefox \
	cmus \
	2>/dev/null

pacman --needed --noconfirm --asdeps -S \
	bash-completion \
	texlive-pictures \
	i3status \
	swaybg \
	swayidle \
	swaylock \
	wl-clipboard \
	2>/dev/null

pacman --needed --noconfirm -S \
	xorg-xwayland \
	wpa_supplicant \
	bluez \
		bluez-utils \
	yt-dlp \
	cifs-utils \
	sl \
	gimp \
	steam fuse2 \
	discord \
	2>/dev/null

path=${0%/*}
cd "$path/install/"
install -Dm644 ./pacman.conf -t /etc/
install -Dm644 ./doas.conf -t /etc/
install -Dm644 ./faillock.conf -t /etc/security/
install -Dm644 ./25-wireless.network -t /etc/systemd/network/
install -Dm644 ./personal.map -t /usr/local/share/kbd/keymaps/
install -Dm644 ./personal-xkb -T /usr/share/X11/xkb/symbols/personal
install -Dm644 <(echo 'en_US.UTF-8 UTF-8') -T /etc/locale.gen
install -Dm644 <(echo 'LANG=en_US.UTF-8') -T /etc/locale.conf
install -Dm644 ./vconsole.conf -t /etc/

ln -sf /usr/share/zoneinfo/Europe/Berlin /etc/localtime
hwclock --systohc
locale-gen
grub-mkconfig -o /boot/grub/grub.cfg

[[ -z $(pacman -T wpa_supplicant) ]] &&
	while IFS= read -r device; do
		config="/etc/wpa_supplicant/wpa_supplicant-$device.conf"
		[[ ! -f $config ]] && install -Dm600 ./wpa_supplicant.conf -T $config
		systemctl enable "wpa_supplicant@$device"
	done <<< "$(networkctl list --json=short \
		| jq -r '.Interfaces[]
			| select(.Type == "wlan")
			| .Name')"
[[ -z $(pacman -T bluez) ]] &&
	systemctl enable bluetooth

systemctl enable \
	systemd-networkd \
	systemd-resolved \
	systemd-timesyncd \
