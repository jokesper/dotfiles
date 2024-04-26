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
			set -l last_status $status
			printf "\n%sKönig%s@%szuhause%s %s%s%s%s%s\n]%s " \
				(set_color -o white) \
				(set_color -o red) \
				(set_color -o white) \
				(set_color normal) \
				(set_color $fish_color_cwd) \
				(prompt_pwd) \
				(set_color normal) \
				(fish_git_prompt) \
				(fish_hg_prompt) \
				(if [ "$last_status" != 0 ]; set_color $fish_color_error; end) \
				(set_color normal)
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
			if command --query lsd
				command lsd $argv
			else
				command ls $argv
			end
		end
end
