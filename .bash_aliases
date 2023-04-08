alias \
	vim=nvim \
	lua=luajit \
	up='doas -n pacman -Syu --' \
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
