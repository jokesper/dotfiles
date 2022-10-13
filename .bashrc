# If not running interactively, don't do anything
[[ $- != *i* ]] && return

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
