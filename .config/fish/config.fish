source "$__fish_config_dir/printers.fish" 2>/dev/null
if not status is-interactive; return; end
if status is-login; exec bash -c '. /etc/profile; eval "$(DISPLAY=:0 ssh-agent -s)"; exec fish'; end

if not set -q DISPLAY; and [ (tty) = '/dev/tty1' ]; exec Hyprland; end

source "$__fish_config_dir/aliases.fish"
source "$__fish_config_dir/shell-gamble.fish"
