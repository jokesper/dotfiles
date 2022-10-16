BEGIN {
    lang = ENVIRON["LANG"]
    if (length(lang) > 0) {

    }
    # fuck local languages
}

/^\[Desktop Entry\]/ {
    show = 1
    relevant = 1
    next
}

/^\[.*\]/ {
    relevant = 0
    next
}

/^Name=/ {
    if (!relevant) next
    sub(/[^=]+=/, "")
    name = $0
}
/^Name\[.*?\]=/ {
    if (!relevant) next
}

/^NoDisplay=true$/ {
    if (!relevant) next
    show = 0
}

# Not supported by kitty
#/^Terminal=true$/ {
#    if (add) 
#}

/^Exec=/ {
    if (!relevant) next
    sub(/[^=]+=/, "")
    sub(/%./, "\"\"") # Field codes are not supported
    exec = $0
}
# TODO: implement URL

ENDFILE {
    if (show) {
        gsub(/\s+/, " ", name)
        gsub(/\s+/, " ", exec)
        print name "\t" exec
    }
}
