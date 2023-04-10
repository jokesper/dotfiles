alias \
	vim=nvim \
	lua=luajit \
	up='doas -n pacman --needed --noconfirm -Sy archlinux-keyring 2>/dev/null; \
		doas -n pacman -Su; \
		~/.dotfiles/user-updates.sh' \
	get_tree='\
		swaymsg -t get_tree \
		| jq ".. \
			| select(.pid? and .visible?) \
			| ({name, class, app_id}) \
			| del(.. | select(. == null))"' \
	ll='la -l' \
	la='ls -A' \
	man='~/.dotfiles/man.sh' \
	file='file -b' \
