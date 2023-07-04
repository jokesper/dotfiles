if not status is-interactive; return; end

function fish_prompt -d 'Write out the prompt'
	printf '%s%s@%s%s:%s%s%s$ ' \
		(set_color cyan) $USER $hostname (set_color normal) \
		(set_color magenta) (basename (prompt_pwd)) (set_color normal) \

end
function fish_greeting -d 'Show unpushed git commits when starting fish'
	git log -n8 @{u}..HEAD -- 2>/dev/null
end
