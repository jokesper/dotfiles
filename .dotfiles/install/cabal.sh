cabalBin="${CABAL_DIR:-.cabal}/bin"

[ -d "$cabalBin" ] && append_path "$cabalBin"

export PATH
