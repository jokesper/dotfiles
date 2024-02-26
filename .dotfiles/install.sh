#!/usr/bin/env bash

set -eu

error() { printf "\e[31;1m%s: %s\e[0m\n" "$0" "$@" >&2; }
warn() { printf "\e[31;1m%s: %s\e[0m\n" "$0" "$@" >&2; }

path=${0%/*}
cd "$path/install/"
install -Dm644 <(printf 'en_US.UTF-8 UTF-8\n') -T /etc/locale.gen
install -Dm644 <(printf 'LANG=en_US.UTF-8\n') -T /etc/locale.conf
install -Dm644 ./personal.kbd -t /etc/kmonad/
install -Dm644 ./reflector-mirrorlist.hook -t /usr/share/libalpm/hooks/
install -Dm644 ./vconsole.conf -t /etc/
install -Dm644 ./pacman.conf -t /etc/
install -Dm644 ./grub -t /etc/default/
install -Dm644 ./doas.conf -t /etc/
install -Dm644 ./faillock.conf -t /etc/security/
install -Dm644 ./25-wireless.network -t /etc/systemd/network/
install -Dm644 ./pam_env.conf -t /etc/security/
install -Dm644 ./udevmon.yaml -t /etc/interception/
install -Dm644 ./keyboard.yaml -t /etc/interception/dual-function-keys/
install -Dm644 ./cabal.sh -t /etc/profile.d/
install -Dm644 ./makepkg.conf -t /etc/

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
			-e 's/.*\(nvidia\).*/\L\1-utils/ip' \
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
	interception-dual-function-keys \
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
	element-desktop \
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
