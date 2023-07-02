# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[[ -z ${DISPLAY:-} ]] && set -eu

config=${XDG_CONFIG_HOME:-$HOME/.config}/bash

export CARGO_HOME=${XDG_DATA_HOME:-$HOME/.local/share}/cargo
mkdir -p ${HISTFILE%/*}

alias \
	doas='doas ' \
	man='man ' \

[[ -x /usr/bin/dircolors ]] && alias \
	ls='ls --color=auto' \
	grep='grep --color=auto' \

[[ -f $config/aliases ]] && \
	. $config/aliases

PS1='\[\e[36m\]\u@\h\[\e[m\]:\[\e[35m\]\W\[\e[m\]\$ '

shopt -s \
	autocd \
	extglob \

# Show unpushed git commits
git log -n8 @{u}..HEAD -- 2>/dev/null || true

complete -cf doas

# Disable after config
set +eu
