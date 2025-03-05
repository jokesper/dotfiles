function fish_prompt -d 'Write out the prompt'
	set -l normal (if [ "$status" != 0 ]
		printf "%s" "$fish_color_error"
	else;
		printf "%s" "$fish_color_normal"
	end)

	if [ $CMD_DURATION -ge 5000 ]
		printf '%sTook %.2fs\n' \
			(set_color $fish_color_warning) \
			(math $CMD_DURATION / 1000)
	end
	printf \
		(printf (if [ -n "$fish_private_mode" ]; echo '%s(%s)%s'; else; echo '%s%s%s'; end) \
			'%s' '%s@%s%s' ':%s%s%s ') \
		(set_color $normal) \
		(set_color $fish_color_user; printf "%s" "$USER") \
		(set_color $fish_color_host; prompt_hostname) \
		(set_color $normal) \
		(set_color $fish_color_cwd; basename (prompt_pwd)) \
		(set_color $normal; if fish_is_root_user; printf '#'; else; printf '$'; end) \
		(set_color $fish_color_normal)
end
