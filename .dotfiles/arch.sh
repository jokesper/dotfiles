#!/usr/bin/env bash

set -eu

error="\e[31;1m$0: %s\e[0m\n"
missing="\e[31;1m$0: Missing parameter '%s'\e[0m\n"
warn="\e[33;1m$0: %s\e[0m\n"

if (( EUID != 0 )); then
	printf "$error" "This script must be run with root privileges" >&2
	exit
fi

# Install via `curl https://raw.githubusercontent.com/jokesper/dotfiles/main/.dotfiles/arch.sh | bash -s <path> <hostname> <kernel> <username>`
[[ -v 1 && $1 == @(-h|--help) ]] && printf \
'Usage: install.sh <path> <hostname> <kernel> <username>
	To create a new installation preferably used with a live
	environment of arch and the first step 1 of the installation guide
	at https://wiki.archlinux.org/title/Installation_guide completed.
	`path`		should be the mount point, e.g. `/mnt`.
	`hostname`	see `hostname(5)`.
	`kernel`	one of stable, hardened, lts, rt, rt-lts, zen
				or an available kernel package.
	`username`	username for a new user in the wheel group.
				the root account will be disabled.
' && exit

[[ ! -v 1 ]] && printf "$missing" path >&2 && exit || path=$(realpath -mL "$1")
if ! mountpoint -q "$path"; then
	printf "$error" "$path is not a mountpoint"
	exit
fi
[[ ! -v 2 ]] && printf "$missing" hostname >&2 && exit || hostname=$2
[[ ! -v 3 ]] && printf "$missing" kernel >&2 && exit || kernel=$3
[[ ! -v 4 ]] && printf "$missing" username  >&2 && exit || username=$4
url="https://github.com/jokesper/dotfiles.git"

case ${kernel:-stable} in
	stable) kernel='linux';;
	hardened | lts | rt | rt-lts | zen)
		kernel="linux-$kernel";;
esac

if which reflector 2>/dev/null; then reflector --country 'Germany,' --save /etc/pacman.d/mirrorlist
else printf "$warn" "reflector not installed" >&2; fi

pacman --needed --noconfirm -Sy archlinux-keyring 2>/dev/null

mkdir -m 0755 -p "$path"/{var/cache/pacman/pkg,lib/pacman,var/log,etc/pacman.d}

mount --mkdir=0555 -t sysfs -o nosuid,noexec,nodev,ro {,"$path"}/sys
mount --mkdir=0555 -t proc -o nosuid,noexec,nodev {,"$path"}/proc
mount --mkdir=1777 -t tmpfs -o size=100M,nosuid,nodev tmpfs "$path"/tmp
mount --mkdir=0755 --rbind -o nosuid {,"$path"}/dev
mount --mkdir=0755 --bind -o nosuid,nodev {,"$path"}/run
uefiDir=/sys/firmware/efi/efivars
[[ -d $uefiDir ]] && mount --rbind {,"$path"}$uefiDir
cp {,"$path"}/etc/resolv.conf

if [[ ! -d $path/etc/pacman.d/gnupg ]]; then
	# Copy or generate new keyring
	[[ -d "/etc/pacman.d/gnupg" ]] \
		&& cp --archive --no-preserve=ownership -T {"$path",}/etc/pacman.d/gnupg \
		|| pacman-key --gpgdir "$path"/etc/pacman.d/gnupg --init
fi
# `unshare` is needed since pacman can spawn child processes (`gnupg`) interferring with unmounting
unshare --fork --pid pacman -r "$path" --noconfirm -Sy base git
cp --archive -T {"$path",}/etc/pacman.d/mirrorlist

genfstab -U "$path" >> "$path/etc/fstab"

chroot "$path" bash -c "set -eu
	passwd --expire --lock root
	printf '$hostname' > /etc/hostname
	useradd -G wheel '$username'
	until passwd '$username' < /dev/tty; do :; done
	runuser - '$username' -c '
		mkdir -m 700 .
		git clone $url .
	' || (printf '$error' "User setup failed..."; bash)
	\"\$(getent passwd '$username' | cut -d: -f6)/.dotfiles/install.sh\"
	runuser - '$username' -c '~/.dotfiles/setup-user.sh'
	bash
"

# NOTE:
# Install kernel after `mkinitcpio` to allow `install.sh` to replace the preset
unshare --fork --pid pacman -r "$path" -S "$kernel"

# Copy `iw` networks if applicable
[[ -f /etc/wpa_supplicant/wpa_supplicant.conf ]] && (
	format-for-wpa_supplicant() {
		printf '
network={
	mac_addr=0
	ssid="%s"
	psk=%s
}' "$1" "$(sed -ne 's/^\s*\(PreSharedKey\|Passphrase\)=\(.*\)$/\2/p' "$1")"
	}
	for network in /var/lib/iwd/[A-Za-z0-9_\-]*.psk; do
		file=${network##*/} # <ssid>.psk
		format-for-wpa_supplicant network "${file%.psk}"
	done
	for network in /var/lib/iwd/=*.psk; do
		ssid=${network##*/=}
		ssid=${ssid%.psk}
		ssid=$(for i in seq 1 2 ${#ssid}; do printf "\x${ssid:i-1:2}"; done)
		format-for-wpa_supplicant network ssid
	done
) >> "$path"/etc/wpa_supplicant/wpa_supplicant.conf

umount --recursive "$path"

reboot
