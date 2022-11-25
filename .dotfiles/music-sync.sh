#!/usr/bin/env bash

cd "$HOME/Music"

lists=$(find \
    -L $(sed 's/:/ /g' <(printf "%q" "$MUSIC:$HOME/Music")) \
    -maxdepth 1 -type f -name \*.list)

#cd "${0%/*}"
#for list in *.list; do
while IFS= read -r list; do
    dir=${list%.*}
    notListed=$(gawk -F "\t" -v sync="yt-dlp -P '$dir' -qxa -" '
        NR==FNR {list[$1] = $2; next}
        match($0, /(.*)\s+\[(.*?)\]/, v) {
            if (v[2] in list)
                {delete list[v[2]]; next}
            notListed[v[2]] = v[1]
            fileNames[v[2]] = $0
        }
        END {
            for (track in list) print track |& sync
            close(sync, "to")

            for (track in notListed)
                print track "\t" notListed[track] "\t" fileNames[track]
        }' \
            "$list" \
            <(ls -1 "$dir" 2> /dev/null))
    if [ -n "$notListed" ]; then
        printf 'The following tracks are not listed in the playlist:\n'
        cut -f2 <<< $notListed
        printf ':: What action to take? [Append/Remove/Ignore] '
        read -r option
        case $option in
            'A'|'a')
                echo "$notListed" >> $list;;
            'R'|'r')
                while IFS= read -r track; do
                    rm "$(cut -f3 <<< "$track")";
                done <<< "$notListed";;
        esac
    fi
done <<< "$lists"
