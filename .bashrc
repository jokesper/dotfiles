# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias \
	doas='doas ' \
	man='man '

if [ -x /usr/bin/dircolors ]; then
	alias \
		ls='ls --color=auto' \
		grep='grep --color=auto'
fi

if [ -f ~/.bash_aliases ]; then
	. ~/.bash_aliases
fi

PS1='\[\e[36m\]\u@\h\[\e[m\]:\[\e[35m\]\W\[\e[m\]\$ '

shopt -s autocd

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
