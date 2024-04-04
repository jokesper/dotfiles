#!/usr/bin/env bash

set -eu

error() { printf "\e[31;1m%s: %s\e[0m\n" "$0" "$*" >&2; }
warn() { printf "\e[31;1m%s: %s\e[0m\n" "$0" "$*" >&2; }

if (( EUID != 0 )); then
	error "This script must be run with root privileges"
	exit
fi

# Find all users and the user (only for single user systems)
users=$(getent passwd \
	| gawk -F: \
		$(sed -ne 's/^\(UID_\(MIN\|MAX\)\)\s\+\([0-9]\+\)$/-v\1=\3/p' /etc/login.defs) '
		{ if (UID_MIN <= $3 && $3 <= UID_MAX \
			&& $7 !~ "(/usr)?/bin/(nologin|false)") {
			printf "%s\n", $1
		} }')
[[ $(wc -l <<< "$users") == 1 ]] && user=$users || user=''


# NOTE:
# install prior to config installation due to file conflicts in preset template
pacman --needed --noconfirm -S mkinitcpio 2>/dev/null

path=${0%/*}
cd "$path/install/"
install -Dm644 <(printf 'en_US.UTF-8 UTF-8\n') -T /etc/locale.gen
install -Dm644 <(printf 'LANG=en_US.UTF-8\n') -T /etc/locale.conf
install -Dm644 <(printf 'root=UUID=%s rw' "$(findmnt -rno UUID /)") -T /etc/cmdline.d/root.conf
# NOTE: Assumution of either `/` being on a physical device or on a luks device
luksRoot=$(dmsetup deps -o devname "$(findmnt -rno SOURCE /)" \
	| grep -Po '(?<=\()[[:alnum:]]+(?=\))' \
	| xargs -I{} lsblk -ndo UUID /dev/{} \
	| sed -e '/^$/d;s/^/rd.luks.name=/;s/$/=root/')
install -Dm644 <(printf '%s' "$luksRoot") -T /etc/cmdline.d/luks-root.conf
install -Dm644 <(printf '[Service]\nExecStart=\nExecStart=%s\n' \
	"$([[ -z "$user" ]] && printf '' || ([[ -n "$luksRoot" ]] \
			&& printf '%s' "-/sbin/agetty -o '-p -f -- \\\\u' --noclear --autologin "$user" %I %TERM" \
			|| printf '%s' "-/sbin/agetty -o '-f -- $user' --noclear"))") \
	-T /etc/systemd/system/getty@tty1.service.d/dotfiles.conf
cp --preserve=mode --recursive --update=older --no-target-directory -- by-path /

if [[ "$(stat -c %d:%i /)" == "$(stat -c %d:%i /proc/$$/root/.)" ]]; then
	ln -rsf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf
fi

no-skipping-warning() { sed -e '/warning: \S\+ is up to date -- skipping/d'; }
pacman --needed --noconfirm -S \
	base \
		linux-firmware \
		$(lscpu | sed -ne 's/^.*\(amd\|intel\).*$/\L\1-ucode/ip') \
		$(lscpu | sed -ne 's/^.*\(intel\|radeon\).*$/\Lvulkan-\1/ip') \
		$(lscpu | sed -ne 's/^.*\(amd\).*$/amdvlk/ip') \
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
	libnotify \
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
	2> >(no-skipping-warning)

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
	2> >(no-skipping-warning)

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
	2> >(no-skipping-warning) \
	&& ([[ ! -f /etc/wpa_supplicant/wpa_supplicant.conf ]] \
		&& install -Dm600 ./wpa_supplicant.conf -t /etc/wpa_supplicant/) \
	&& while IFS= read -r device; do
		[[ -z $device ]] && continue
		config="/etc/wpa_supplicant/wpa_supplicant-$device.conf"
		[[ ! -f $config ]] && install -Dm600 /etc/wpa_supplicant/wpa_supplicant.conf -T "$config"
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
