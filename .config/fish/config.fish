if not status is-interactive; return; end
if status is-login; exec bash -c '. /etc/profile; exec fish'; end

if not set -q DISPLAY; and [ (tty) = '/dev/tty1' ]; exec Hyprland; end

function fish_prompt -d 'Write out the prompt'
	printf \
		(printf (if test -n "$fish_private_mode"; echo '(%s)%s'; else; echo '%s%s'; end) \
			'%s%s@%s%s%s' ':%s%s%s%s ') \
		(set_color $fish_color_user) $USER \
		(set_color $fish_color_host) $hostname \
		(set_color $fish_color_normal) \
		(set_color $fish_color_cwd) (basename (prompt_pwd)) \
		(set_color $fish_color_normal) \
		(if fish_is_root_user; printf '#'; else; printf '$'; end)
end
function fish_mode_prompt -d 'Disable Indicator for modes'; end
function fish_greeting -d 'Show unpushed git commits when starting fish'
	git log -n8 @{u}..HEAD -- 2>/dev/null
end

source "$__fish_config_dir/aliases.fish"
