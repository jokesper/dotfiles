#!/usr/bin/env bash

set -eu

typst=${XDG_DATA_HOME:-$HOME/.local/share}/dotfiles/typst.git
symbols=crates/typst/src/symbols

luajit -- <(printf "%s" "
	local function to_fcitx5(res, tbl, key)
		if type(res) == 'string' then
			local mod, err = loadfile(res or false, 't', {})
			if not mod then error(err) end
			res = {}
			to_fcitx5(res, (mod()), '')
			local out = io.open(tbl, 'w')
			out:write(table.concat(res, '\n'))
			out:close()
		else
			for k, v in pairs(tbl) do
				if type(v) ~= 'table' then
					table.insert(res, key .. k .. ' ' .. v)
				else to_fcitx5(res, v, key .. k .. '.') end
			end
		end
	end
	local sym = ...
	local path = '$XDG_DATA_HOME/fcitx5/data/quickphrase.d/'
	to_fcitx5(sym, path .. 'sym.mb')
") <(git --git-dir="$typst" show HEAD:$symbols/sym.rs \
	| sed \
		-e '0,/const SYM/d' -e '/^};/d' \
		-e 's/#\[.*\]//g' \
		-e '/\/\//d' \
		-e 's/\[/{/g' -e 's/\]/}/g' \
		-e 's/\([A-Za-z.]\+\):/["\1"] =/g' \
	| sed -e '1 i\return {' -e '$a\}' \
)
