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
function down -d 'Quick alias to poweroff'; ~/.dotfiles/stop-and-shutdown.hs & disown; end
function ls -w ls -d 'Color `ls` output'; command ls --color=auto -H $argv; end
function grep -w grep -d 'Color `grep` output'; command grep --color=auto $argv; end
function ll -w ls -d '`ls` but with long listing format'; ls -Alh $argv; end
function la -w ls -d '`ls` but listing every item'; ls -A $argv; end
function file -w file -d '`file` but with the `--brief` option'; command file -b $argv; end
function less -w less -d '`less` but with better defaults'; command less -FiRX $argv; end
function :q -w exit -d 'I try my best but sometimes...'; exit; end
function gca -w 'git commit' -d '`git commit --amend`'; command git commit --amend -S --no-edit $argv; end
function gc -w 'git commit' -d '`git commit`'; command git commit $argv; end
function gs -w 'git status' -d '`git status`'; command git status $argv; end
function gl -w 'git log' -d '`git log`'; command git log $argv; end
function gsh -w 'git show' -d '`git show`'; command git show $argv; end
function gls -w 'git log' -d '`git log -n20`'; command git log -n20 $argv; end
function gd -w 'git diff' -d '`git diff`'; command git diff $argv; end
function ga -w 'git add' -d '`git add`'; command git add $argv; end
function gap -w 'git commit' -d '`git add -p`'; command git add -p $argv; end
function gr -w 'git rebase' -d '`git rebase`'; command git rebase $argv; end
function grc -w 'git rebase' -d '`git rebase --continue`'; command git rebase --continue $argv; end
function gri -w 'git rebase' -d '`git rebase -i`'; command git rebase -i $argv; end
function gps -w 'git push' -d '`git push`'; command git push $argv; end
function gpf -w 'git push' -d '`git push --force`'; command git push --force-with-lease --force-if-includes $argv; end
function gpl -w 'git pull' -d '`git pull`'; command git pull $argv; end

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
