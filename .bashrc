# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'

if [ -f ~/.bash_aliases ]; then
    . ~/.bash_aliases
fi

PS1='\e[1;32m\u@\h\e[0m:\e[1;34m\W\e[0m\$ '
