#!/usr/bin/env bash

set -eu

data=${XDG_DATA_HOME:-$HOME/.local/share}/dotfiles
profile=~/.mozilla/firefox/dotfiles

git --git-dir="$data/user.js.git" show HEAD:user.js \
	| sed -z \
		-e 's!/\*\([^*]\|\*[^/]\)*\*/!!g' \
		-e 's!\([\n;]\)[[:space:]]*//[^\n]*!\1!g' \
		-e 's/\n\+/\n/g' \
	| cat - <(luajit -- <(printf "%s" "
		local _json, override = ...
		local json = loadfile((_json))()
		local func, err = loadfile(override, 't')
		if func == nil then error(err) end
		function to_user_js(res, tbl, key)
			for k,v in pairs(tbl) do
				if type(v) ~= 'table' then
					table.insert(res, ('user_pref(%s, %s);')
						:format(json.encode(key .. k), json.encode(v)))
				else to_user_js(res, v, key .. k .. '.') end
			end
			return res
		end
		print(table.concat(to_user_js({}, func(json), ''), '\n'))
	") <(git --git-dir="$data/json.lua.git" show HEAD:json.lua) "$profile/user-override.lua") \
	> "$profile/user.js"
