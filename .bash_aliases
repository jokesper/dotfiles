alias \
    vim=nvim \
    get_tree='\
        swaymsg -t get_tree \
        | jq ".. \
            | select(.pid? and .visible?) \
            | ({name, class, app_id}) \
            | del(.. | select(. == null))"' \
