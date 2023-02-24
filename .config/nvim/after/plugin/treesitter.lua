require 'nvim-treesitter.configs'.setup {
	ensure_installed = {
		'bash', 'awk',
		'lua', 'rust',
		'markdown', 'latex',
	},
	sync_install = false,
	auto_install = true,
	highlight = {
		enable = true,
		additional_vim_regex_highlighting = false,
	},
}
