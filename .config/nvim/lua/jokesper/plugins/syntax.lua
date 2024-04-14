return {
	{
		'nvim-treesitter/nvim-treesitter',
		build = function() require 'nvim-treesitter.install'.update { with_sync = true } () end,
		main = 'nvim-treesitter.configs',
		opts = {
			ensure_installed = {
				'vimdoc',
				'gitignore', 'gitcommit',
				'diff',
				'fish', 'bash', 'awk',
				'lua', 'rust', 'haskell',
				'markdown', 'latex', 'typst',
			},
			sync_install = false,
			auto_install = true,
			highlight = {
				enable = true,
				additional_vim_regex_highlighting = false,
			},
			indent = { enable = true },
		},
	},
	{ 'nvim-treesitter/nvim-treesitter-context' },
	{ 'kmonad/kmonad-vim' },
}
