if not status is-interactive; return; end
if status is-login; exec bash -c '. /etc/profile; eval "$(DISPLAY=:0 ssh-agent -s)"; exec fish'; end

if not set -q DISPLAY; and [ (tty) = '/dev/tty1' ]; exec Hyprland; end

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
function fish_mode_prompt -d 'Disable Indicator for modes'; end
function show_unpushed -v PWD -d 'Show unpushed git commits'
	git log -n8 @{u}..HEAD -- 2>/dev/null
end
function fish_greeting -d 'Show unpushed git commits when starting fish'
	show_unpushed
end

source "$__fish_config_dir/aliases.fish"
source "$__fish_config_dir/shell-gamble.fish"
