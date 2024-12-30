function which -w which -w 'pacman -Qi' -d 'Show more information about the file'
	if isatty stdout
		pacman -Qi $argv 2>/dev/null
		or pacman -Qo $argv 2>/dev/null
		or command which $argv
	else
		command which $argv
	end
end
function doasedit -d 'An alias to edit files without running nvim as root'
	command doas ~/.dotfiles/doasedit.sh nvim "$argv"
end
function up -d 'Upgrade the entire system'
	doas -n pacman --needed --noconfirm -Sy archlinux-keyring 2>/dev/null
	and doas -n pacman -Su
	and ~/.dotfiles/user-updates.sh
end
function down -d 'Quick alias to poweroff'; ~/.dotfiles/stop-and-shutdown.sh; end
function ls -w ls -d 'Color `ls` output'; command ls --color=auto -H $argv; end
function grep -w grep -d 'Color `grep` output'; command grep --color=auto $argv; end
function ll -w ls -d '`ls` but with long listing format'; ls -Alh $argv; end
function la -w ls -d '`ls` but listing every item'; ls -A $argv; end
function file -w file -d '`file` but with the `--brief` option'; command file -b $argv; end
function less -w less -d '`less` but with better defaults'; command less -FiRX; end
function :q -w exit -d 'I try my best but sometimes...'; exit; end
function h -w hoogle -d 'Classic hoogle search with extras'
	set arg (string join " " -- $argv)
	if test -z "$arg"; read -P'λ> ' arg; end
	command hoogle search --count=10 --json \
		--database="$XDG_DATA_HOME"/hoogle/haskell.hoo \
		-- "$arg" \
		| jq -rR "fromjson? | .[] | [ \
			\"$(set_color green)\(.package.name // \"\")\", \
			\"$(set_color cyan)\(.module.name // \"\")\", \
			\"$(set_color magenta)\(.item)$(set_color normal)\" \
			] | @tsv" \
		| column -ts\t
end
function newpass -d 'Generate password'
	openssl rand -base64 30 | tee /dev/stderr | wl-copy
end

function hl -w hledger -d 'HLedger for ongoing finances'
	if [ "$argv" = check ]
		hledger --file=(date +%Y).journal check --strict \
			ordereddates \
			tags \

	else
		hledger \
			--pretty \
			--file=(date +%Y).journal \
			--begin=(date +%Y) \
			$argv \
			2>/dev/null \
		|| hledger --file=(date +%Y).journal $argv \

	end
end

function sshlpr -d 'Print over SSH';
	pdfunite $argv[3..] /dev/stdout \
		| ssh "$argv[1]" lpr -P "$argv[2]"
end

#function <...> -d 'Print somewhere'; sshlpr <host> <printer> $argv; end
