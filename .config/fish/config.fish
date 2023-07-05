if not status is-interactive; return; end

export PATH="$PATH:$CARGO_HOME/bin"

if not set -q DISPLAY; and [ (tty) = '/dev/tty1' ]; exec sway --unsupported-gpu; end

function fish_prompt -d 'Write out the prompt'
	printf '%s%s@%s%s:%s%s%s$ ' \
		(set_color cyan) $USER $hostname (set_color normal) \
		(set_color magenta) (basename (prompt_pwd)) (set_color normal) \

end
function fish_mode_prompt -d 'Disable Indicator for modes'; end
function fish_greeting -d 'Show unpushed git commits when starting fish'
	git log -n8 @{u}..HEAD -- 2>/dev/null
end

source "$__fish_config_dir/aliases.fish"
