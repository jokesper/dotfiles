alias \
    vim=nvim \
    sudo='sudo --preserve-env=HOME,EDITOR '
    get_tree="swaymsg -t get_tree | grep -oE --color=never '\"(app_id|class)\": \".*\"'"
