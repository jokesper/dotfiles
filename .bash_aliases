alias \
    vim=nvim \
    lua=luajit \
    pacman="pacman --hookdir=$HOME/.config/pacman/hooks/" \
    yay="yay --hookdir=$HOME/.config/pacman/hooks/" \
    get_tree='\
        swaymsg -t get_tree \
        | jq ".. \
            | select(.pid? and .visible?) \
            | ({name, class, app_id}) \
            | del(.. | select(. == null))"' \
    ll='la -l' \
    la='ls -A' \
