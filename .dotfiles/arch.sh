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

if which reflector 2>/dev/null; then reflector --country 'Germany,' >/dev/null
else printf "$warn" "reflector not installed" >&2; fi

pacman --needed --noconfirm -Sy archlinux-keyring 2>/dev/null

genfstab -U "$path" >> "$path/etc/fstab"

mount --mkdir=0555 -t sysfs {,"$path"}/sys
mount --mkdir=0555 -t proc {,"$path"}/proc
mount --mkdir=1777 -t tmpfs -o size=100Mi,mode=1777 "$path"/tmp
mount --mkdir=0755 --rbind {,"$path"}/dev
uefiDir=/sys/firmware/efi/efivars
[[ -d $uefiDir ]] && mount --rbind {,"$path"}$uefiDir
cp {,"$path"}/etc/resolv.conf

# Setup pacman
mkdir -m 0755 -p "$path"/var/{cache/pacman/pkg,lib/pacman,log} "$path"/{dev,run,etc/pacman.d}
[[ ! -d $path/etc/pacman.d/gnupg ]] && pacman-key --gpgdir "$path"/etc/pacman.d/gnupg --init
unshare --fork --pid pacman -r "$path" -Sy base "$kernel" git
cp -a {,"$path"}/etc/pacman.d/mirrorlist

chroot "$path" bash -c "set -eu
	passwd --expire --lock root
	printf '$hostname' > /etc/hostname
	useradd -mG wheel -s /bin/bash '$username'
	until passwd '$username' < /dev/tty; do :; done
	runuser - '$username' -c '
		shopt -s dotglob nullglob
		rm -r ~/*
		git clone $url .
	\"\$(getent passwd '$username' | cut -d: -f6)/.dotfiles/install.sh\"
	runuser - '$username' -c ~/.dotfiles/setup-user.sh
	bash"
umount --recursive "$path"

# TODO:
# read connected wireless networks from `/var/lib/iwd/*.psk` and add them to
# wpa_supplicant.
# Also set `mac_addr=0` since we probably don't want randomized mac addresses
# for them

reboot
