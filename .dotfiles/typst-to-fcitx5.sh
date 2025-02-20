#!/usr/bin/env bash

set -eu

data=${XDG_DATA_HOME:-$HOME/.local/share}
typst=$data/dotfiles/typst.git
dest=$data/fcitx5/data/quickphrase.d/typst

"${0%/*}/typst-to-fcitx5.hs"

# We ignore the \u{...} parts, since they mostly contain spaces anyways
git --git-dir="$typst" show HEAD:crates/typst-syntax/src/ast.rs \
	| sed -ne '/const LIST/,/\];$/p' \
	| sed -ne 's/\s*("\([^" ]\+\)", '\''\([^'\'']\+\)'\''),.*$/\1 \2/p' \
	| sed -e 's/^\(\S\+\) \\u{\([0-9A-Fa-f]\+\)}/\1 \\u\2/' \
	| xargs -0I{} printf {} \
	| sort -u \
	> "$dest"-shorthand.mb
