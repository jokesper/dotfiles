[[ -f ~/.bashrc ]] && . ~/.bashrc
[[ -z ${DISPLAY:-} && $(tty) = '/dev/tty1' ]] && exec sway
