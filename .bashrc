# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias \
    sudo='sudo --preserve-env=HOME,EDITOR ' \
    man='man '

if [ -x /usr/bin/dircolors ]; then
    alias \
        ls='ls --color=auto' \
        grep='grep --color=auto'
fi

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

PS1='\[\e[32m\]\u@\h\[\e[0m\]:\[\e[34m\]\w\[\e[0m\]\$ '

export EDITOR=nvim
export MOZ_ENABLE_WAYLAND=1

#export XDG_CONFIG_HOME="$HOME/.config"
#export XDG_CACHE_HOME="$HOME/.cache"
export XDG_DATA_HOME="$HOME/.local/share"
#export XDG_STATE_HOME="$HOME/.local/state"
export XDG_DATA_DIRS="/usr/local/share:/usr/share"
