function which -w which -w 'pacman -Qi' -d 'Show more information about the file'
	pacman -Qi $argv 2>/dev/null
	or pacman -Qo $argv 2>/dev/null
	or command which $argv
end
function doasedit -d 'An alias to edit files without running nvim as root'
	command doas ~/.dotfiles/doasedit.sh nvim "$argv"
end
function up -d 'Upgrade the entire system'
	doas -n pacman --needed --noconfirm -Sy archlinux-keyring 2>/dev/null
	and doas -n pacman -Su
	and ~/.dotfiles/user-updates.sh
end
function down -w poweroff -d 'Quick alias to poweroff'; poweroff $argv; end
function ls -w ls -d 'Color `ls` output'; command ls --color=auto -H $argv; end
function grep -w grep -d 'Color `grep` output'; command grep --color=auto $argv; end
function ll -w ls -d '`ls` but with long listing format'; ls -Alh $argv; end
function la -w ls -d '`ls` but listing every item'; ls -A $argv; end
function file -w file -d '`file` but with the `--brief` option'; command file -b $argv; end
function h -w hoogle -d 'Classic hoogle search with extras'
	set arg (string join " " -- $argv)
	if test -z "$arg"; read -P'λ> ' arg; end
	command hoogle search --count=10 --json -- "$arg" \
		| jq -rR "fromjson? | .[] | [ \
			\"$(set_color green)\(.package.name // \"\")\", \
			\"$(set_color cyan)\(.module.name // \"\")\", \
			\"$(set_color magenta)\(.item)$(set_color normal)\" \
			] | @tsv" \
		| column -ts\t
end
function newpass -d 'Generate password'
	openssl rand -base64 15 | tee /dev/stderr | wl-copy
end
