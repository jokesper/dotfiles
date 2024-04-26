switch (date | md5sum | head -c2)
	case 10
		if command --query sl; function ls; command sl -lGw; end; end
	case 11
		if command --query python && command --query ghci
			function python; command ghci; end
		end
	case 12
		printf "sleep 0.1\n" >> "$__fish_config_dir/config.fish"
	case 13
		find "$__fish_config_dir" -type f -name \*.fish \
			| shuf -n1 \
			| xargs -I{} -- bash -c 'printf "# Turtles" >> "{}"'
	case 14
		function fish_prompt
			printf \
				(printf (if test -n "$fish_private_mode"; echo '(%s)%s'; else; echo '%s%s'; end) \
					'%sKönig@%szuhause%s' ':%s%s%s%s ') \
				(set_color $fish_color_user) \
				(set_color $fish_color_host) \
				(set_color $fish_color_normal) \
				(set_color $fish_color_cwd) (basename (prompt_pwd)) \
				(set_color $fish_color_normal) \
				(if fish_is_root_user; printf '#'; else; printf '$'; end)
		end
	case 15
		function fish_greeting
			printf "%sTurtles%s\n" (set_color -o green) (set_color normal)
		end
		if set -q fish_private_mode
			printf "... aber versteckt\n"
		end
	case 16
		command xdg-open 'https://en.wikipedia.org/wiki/Turtle'
	case 17
		function ls
			printf "sleep 0.01\n" >> "$__fish_config_dir/config.fish"
			command ls --color=auto -H $argv
		end
end
