require 'nvim-treesitter.configs'.setup {
	ensure_installed = {
		'vimdoc',
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
	indent = {enable = true},
}
