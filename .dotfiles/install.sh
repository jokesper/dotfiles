#!/usr/bin/env bash

set -eu

error() { printf "\e[31;1m%s: %s\e[0m\n" "$0" "$*" >&2; }
warn() { printf "\e[31;1m%s: %s\e[0m\n" "$0" "$*" >&2; }

if (( EUID != 0 )); then
	error "This script must be run with root privileges"
	exit
fi

# NOTE:
# install prior to config installation due to file conflicts in preset template
pacman --needed --noconfirm -S mkinitcpio 2>/dev/null

path=${0%/*}
cd "$path/install/"
install -Dm644 <(printf 'en_US.UTF-8 UTF-8\n') -T /etc/locale.gen
install -Dm644 <(printf 'LANG=en_US.UTF-8\n') -T /etc/locale.conf
install -Dm644 <(printf 'root=UUID=%s rw' "$(findmnt -rno UUID /)") -T /etc/cmdline.d/root.conf
cp --preserve=mode --recursive --update=older --no-target-directory -- by-path /

if [[ "$(stat -c %d:%i /)" == "$(stat -c %d:%i /proc/$$/root/.)" ]]; then
	ln -rsf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf
fi

pacman --needed --noconfirm -S \
	base \
		linux-firmware \
		$(lscpu | sed -n 's/.*\(amd\|intel\).*/\L\1-ucode/ip') \
		$(lscpu | sed -n \
			-e 's/.*\(intel\|radeon\).*/\Lvulkan-\1/ip' \
			-e 's/.*\(amd\).*/amdvlk/ip' \
		) \
		gcc \
		git \
		jq \
		which \
		xdg-utils \
		fakeroot \
		make \
		patch \
		reflector \
	apparmor \
	wireplumber \
		pipewire-pulse \
	brightnessctl \
	man-db \
	doas \
	neovim \
		ripgrep \
		texlive-meta \
			texlab \
		typst \
	hyprland \
		wl-clipboard \
		xdg-desktop-portal-hyprland \
		`#polkit-kde-agent` \
		swaylock \
		swayidle \
		waybar \
		wofi \
		grim slurp \
	kitty \
		ttf-firacode-nerd \
		fish \
	rust \
		libgit2 \
		rust-analyzer \
	cabal-install \
		haskell-language-server \
		hoogle \
	firefox \
	cmus \
	2>/dev/null

pacman --needed --noconfirm -S \
	xorg-xwayland \
	bluez \
		bluez-utils \
	yt-dlp \
	cifs-utils \
	gnupg \
	sl \
	gimp \
	steam fuse2 \
	element-desktop \
	2>/dev/null

ln -sf /usr/share/zoneinfo/Europe/Berlin /etc/localtime
hwclock --systohc
locale-gen
if [[ -d /sys/firmware/efi/efivars ]]; then
	bootctl install
else
	error 'BIOS systems are currently not supported.'
	warn 'Please install manually'
	fish
fi

lspci | grep 'Network controller' >/dev/null \
	&& pacman --needed --noconfirm -S \
	wpa_supplicant \
	2>/dev/null \
	&& while IFS= read -r device; do
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
	apparmor \
