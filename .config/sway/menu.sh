#!/usr/bin/env bash
find -L $(sed 's/:/\/applications /g' <<< "$XDG_DATA_HOME:$XDG_DATA_DIRS:") -type f -name \*.desktop \
    | xargs gawk -f ~/.config/sway/parse-desktop-entry.awk \
    | sort \
    | gawk -F "\t" -v menu='bemenu -ipλ' '
        {
            print $1 |& menu
            cmds[$1] = $2
        }

        END {
            close(menu, "to")
            menu |& getline name
            print cmds[name]
        }'
