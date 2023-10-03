#!/usr/bin/env bash

set -eu

error() { printf "\e[31;1m%s: %s\e[0m\n" "$0" "$@" >&2; }
warn() { printf "\e[31;1m%s: %s\e[0m\n" "$0" "$@" >&2; }

path=${0%/*}
cd "$path/install/"
install -Dm644 <(echo 'en_US.UTF-8 UTF-8') -T /etc/locale.gen
install -Dm644 <(echo 'LANG=en_US.UTF-8') -T /etc/locale.conf
install -Dm644 ./personal.map -t /usr/local/share/kbd/keymaps/
install -Dm644 ./personal-xkb -T /usr/share/X11/xkb/symbols/personal
install -Dm644 ./vconsole.conf -t /etc/
install -Dm644 ./pacman.conf -t /etc/
install -Dm644 ./grub -t /etc/default/
install -Dm644 ./doas.conf -t /etc/
install -Dm644 ./faillock.conf -t /etc/security/
install -Dm644 ./25-wireless.network -t /etc/systemd/network/
install -Dm644 ./pam_env.conf -t /etc/security/
install -Dm644 ./udevmon.yaml -t /etc/interception/
install -Dm644 ./keyboard.yaml -t /etc/interception/dual-function-keys/

if [[ "$(stat -c %d:%i /)" == "$(stat -c %d:%i /proc/$$/root/.)" ]]; then
	ln -rsf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf
fi

pacman --needed --noconfirm -S \
		gcc \
		jq \
		which \
		xdg-utils \
		fakeroot \
		make \
		patch \
	grub \
	wireplumber \
		pipewire-pulse \
	brightnessctl \
	man-db \
	doas \
	neovim \
		ripgrep \
		texlive-meta \
			texlab \
	sway \
		bemenu-wayland \
		grim slurp \
	kitty \
		ttf-firacode-nerd \
		fish \
	rustup \
	cabal-install \
		haskell-language-server \
		hoogle \
	firefox \
	cmus \
	interception-dual-function-keys \
	2>/dev/null

pacman --needed --noconfirm --asdeps -S \
	i3status \
	swaybg \
	swayidle \
	swaylock \
	mako \
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

ln -sf /usr/share/zoneinfo/Europe/Berlin /etc/localtime
hwclock --systohc
locale-gen
if [[ -d /sys/firmware/efi/efivars ]]; then
	pacman --needed --noconfirm -S efibootmgr 2>/dev/null
	grub-install --target=x86_64-efi --efi-directory=/boot --removable
else
	error 'BIOS systems are currently not supported.'
	warn 'Please install manually'
	fish
fi
grub-mkconfig -o /boot/grub/grub.cfg

[[ -z $(pacman -T wpa_supplicant) ]] &&
	while IFS= read -r device; do
		config="/etc/wpa_supplicant/wpa_supplicant-$device.conf"
		[[ ! -f $config ]] && install -Dm600 ./wpa_supplicant.conf -T "$config"
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
	udevmon \
