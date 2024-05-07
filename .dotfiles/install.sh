#!/usr/bin/env bash

set -eu

error() { printf "\e[31;1m%s: %s\e[0m\n" "$0" "$*" >&2; }
warn() { printf "\e[33;1m%s: %s\e[0m\n" "$0" "$*" >&2; }
no-skipping-warning() { sed -e '/warning: \S\+ is up to date -- skipping/d'; }
no-skipping-warning() { sed -e '/warning: \S\+ is up to date -- skipping/d'; }
ignore-missing-package() { sed -e '/error: package \S\+ was not found/d'; }

if (( EUID != 0 )); then
	error "This script must be run with root privileges"
	exit
fi

path=${0%/*}
cd "$path/install/"

# ===== Packages =====
packages=$(printf "%s\n" \
	base \
		linux-firmware \
		$(lscpu | sed -ne 's/^.*\(amd\|intel\).*$/\L\1-ucode/ip') \
		$(lscpu | sed -ne 's/^.*\(intel\|radeon\).*$/\Lvulkan-\1/ip') \
		$(lscpu | sed -ne 's/^.*\(amd\).*$/amdvlk/ip') \
		$(lspci | sed -ne 's/^.*Network controller.*$/wpa_supplicant/p') \
		gcc \
		git \
			git-delta \
		jq \
		which \
		xdg-utils \
		fakeroot \
		make \
		patch \
		reflector \
	mkinitcpio \
	plymouth \
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
	kitty \
		ttf-firacode-nerd \
		fish \
	rust \
		rust-analyzer \
	cabal-install \
		haskell-language-server \
		hoogle \
	firefox \
	cmus \
	bluez \
	yt-dlp \
	cifs-utils \
	gnupg \
	sl \
	gimp \
	steam fuse2 \
	element-desktop \
	| sort --unique)

add-package() { packages=$(printf "%s\n" "$packages" "$@"); }
scripts=()
while IFS= read -rd '' script; do
	name=${script##*/}
	dependencies=${name%.*}
	[[ -z "$(comm -23 -- \
		<(sed -e 's/\s\+/\n/g' <<< "$dependencies") \
		<(sort --unique <<< "$packages"))" ]] \
		&& scripts+=( "$script" )
done < <(find when-installed -type f -executable -print0)

for script in "${scripts[@]}"; do before() { true; }; . "$script"; before; done
packages=$(sort --unique <<< "$packages")

packageCache=/var/cache/dotfiles/packages

[[ ! -f "$packageCache" ]] \
	&& mkdir -p "${packageCache%/*}" \
	&& touch "$packageCache"

deltaPackages=$(comm -3 <(printf "%s" "$packages") "$packageCache")
addPackages=$(sed -ne 's/^\(\S\+\)$/\1/p' <<< "$deltaPackages")
remPackages=$(sed -ne 's/^\t\(\S\+\)$/\1/p' <<< "$deltaPackages")

[[ -n "$addPackages" ]] \
	&& pacman --needed --noconfirm -S $addPackages \
	2> >(no-skipping-warning)
[[ -n "$remPackages" ]] \
	&& pacman -Qq $remPackages \
	2> >(ignore-missing-package) \
	| xargs pacman --noconfirm -Rsu --

printf "%s" "$packages" > "$packageCache"

# ===== Configuration =====

# Find all users and the user (only for single user systems)
users=$(getent passwd \
	| gawk -F: \
		$(sed -ne 's/^\(UID_\(MIN\|MAX\)\)\s\+\([0-9]\+\)$/-v\1=\3/p' /etc/login.defs) '
		{ if (UID_MIN <= $3 && $3 <= UID_MAX \
			&& $7 !~ "(/usr)?/bin/(nologin|false)") {
			printf "%s\n", $1
		} }')
[[ $(wc -l <<< "$users") == 1 ]] && user=$users || user=''

install -Dm644 <(printf 'en_US.UTF-8 UTF-8\n') -T /etc/locale.gen
install -Dm644 <(printf 'LANG=en_US.UTF-8\n') -T /etc/locale.conf
install -Dm644 <(printf 'root=UUID=%s rw' "$(findmnt -rno UUID /)") -T /etc/cmdline.d/root.conf
# NOTE: Assumution of either `/` being on a physical device or on a luks device
luksRoot=$(dmsetup deps -o devname "$(findmnt -rno SOURCE /)" \
	2>/dev/null \
	| grep -Po '(?<=\()[[:alnum:]]+(?=\))' \
	| xargs -I{} lsblk -ndo UUID /dev/{} \
	| sed -e '/^$/d;s/^/rd.luks.name=/;s/$/=root/')
install -Dm644 <(printf '%s' "$luksRoot") -T /etc/cmdline.d/luks-root.conf
install -Dm644 <(printf '[Service]\nExecStart=\nExecStart=%s\n' \
	"$([[ -z "$user" ]] && printf '' || ([[ -n "$luksRoot" ]] \
			&& printf '%s' "-/sbin/agetty -o '-p -f -- \\\\u' --noclear --autologin $user %I \$TERM" \
			|| printf '%s' "-/sbin/agetty -o '-p -- $user' --noclear --skip-login - \$TERM"))") \
	-T /etc/systemd/system/getty@tty1.service.d/dotfiles.conf
cp --preserve=mode --recursive --update=older --no-target-directory -- by-path /

if [[ "$(stat -c %d:%i /)" == "$(stat -c %d:%i /proc/$$/root/.)" ]]; then
	ln -rsf /run/systemd/resolve/stub-resolv.conf /etc/resolv.conf
fi

for script in "${scripts[@]}"; do after() { true; }; . "$script"; after; done

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

systemctl enable \
	systemd-networkd \
	systemd-resolved \
	systemd-timesyncd \
	apparmor \
