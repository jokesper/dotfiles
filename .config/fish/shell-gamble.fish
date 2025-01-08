if set -q FISH_RECURSIVE_GAMBLE; set --erase FISH_RECURSIVE_GAMBLE; return; end

switch (date | md5sum | head -c2)
	case 08; FISH_RECURSIVE_GAMBLE=1 exec bash -c 'script -qc fish /dev/null | stdbuf -o0 tr oilzeasgt 011234567'
	case 09
		functions -c ls __ls_orig
		set __fake_pwd "$PWD"
		function cd; set __fake_pwd "$__fake_pwd/$argv[1]"; end
		function ls; __ls_orig "$__fake_pwd/$argv[1]"; end
	case 10; if command --query sl; function ls; command sl -lGw; end; end
	case 11; if command --query ghci; exec ghci -v0 -ghci-script (printf ':set prompt "%s"\n' (fish_prompt) | psub); end
	case 12; printf "sleep 0.1\n" >> "$__fish_config_dir/config.fish"
	case 13
		find "$__fish_config_dir" -type f -name \*.fish \
			| shuf -n1 \
			| xargs -I{} -- bash -c 'printf "# Turtles\n" >> "{}"'
	case 14
		functions -c fish_prompt __fish_prompt_orig
		function prompt_hostname; printf 'zuhause'; end
		function fish_prompt; USER="König" __fish_prompt_orig; end
	case 15
		functions -c fish_greeting __fish_greeting_orig
		function fish_greeting; __fish_greeting_orig; printf "%sTurtles%s\n" (set_color -o green) (set_color normal); end
		if set -q fish_private_mode; printf "... aber versteckt\n"; end
	case 16; command xdg-open 'https://en.wikipedia.org/wiki/Turtle' & disown
	case 17
		functions -c ls __ls_orig
		function ls; printf "sleep 0.01\n" >> "$__fish_config_dir/config.fish"; __ls_orig; end
end
