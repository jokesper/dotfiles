# If not running interactively, don't do anything
[[ $- != *i* ]] && return

set -eu

config=${XDG_CONFIG_HOME:-$HOME/.config}/bash
state=${XDG_STATE_HOME:-$HOME/.local/state}/bash

mkdir -p $state
export HISTFILE=$state/history

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

export EDITOR=nvim
export MOZ_ENABLE_WAYLAND=1

#export XDG_CONFIG_HOME="$HOME/.config"
#export XDG_CACHE_HOME="$HOME/.cache"
#export XDG_DATA_HOME="$HOME/.local/share"
#export XDG_STATE_HOME="$HOME/.local/state"
#export XDG_DATA_DIRS="/usr/local/share:/usr/share"

# Show unpushed git commits
git log -n8 @{u}..HEAD -- 2>/dev/null

complete -cf doas

# Disable after config
set +eu
