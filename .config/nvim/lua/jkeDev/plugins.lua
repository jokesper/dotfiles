local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system { 'git', 'clone',
		'--filter=blob:none',
		'https://github.com/folke/lazy.nvim.git',
		'--branch=stable',
		lazypath }
end
vim.opt.rtp:prepend(lazypath)
require 'lazy'.setup {
	{
		'nvim-treesitter/nvim-treesitter',
		build = function() require 'nvim-treesitter.install'.update { with_sync = true } () end,
	},
	'nvim-treesitter/nvim-treesitter-context',
	{ 'lervag/vimtex', ft = 'tex', lazy = true },
	{
		'nvim-telescope/telescope.nvim',
		branch = '0.1.x',
		dependencies = 'nvim-lua/plenary.nvim',
	},
	{
		'glacambre/firenvim',
		cond = vim.g.started_by_firenvim,
		build = function()
			require 'lazy'.load { plugins = 'firenvim', wait = true }
			vim.fn['firenvim#install'](0)
		end
	},
	{
		'lukas-reineke/indent-blankline.nvim',
		opts = {
			char = '▎',
			context_char_list = { '▎' },
			show_current_context = true,
			show_current_context_start = true,
		},
	},
	{
		'jkeDev/cloak.nvim',
		opts = {
			patterns = {
				{
					file_pattern = '*wpa_supplicant*.conf',
					cloak_pattern = '(psk=).+',
					replace = '%1',
				},
			},
		},
	},

	{
		'VonHeikemen/lsp-zero.nvim',
		branch = 'v2.x',
		dependencies = {
			{ 'neovim/nvim-lspconfig' },
			{
				'williamboman/mason.nvim',
				build = function() pcall(vim.cmd, 'MasonUpdate') end
			},
			{ 'williamboman/mason-lspconfig.nvim' },
			{ 'hrsh7th/nvim-cmp' },
			{ 'hrsh7th/cmp-cmdline' },
			{ 'hrsh7th/cmp-buffer' },
			{ 'hrsh7th/cmp-calc' },
			{ 'FelipeLema/cmp-async-path' },
			{ 'hrsh7th/cmp-nvim-lua' },
			{ 'hrsh7th/cmp-nvim-lsp' },
			{ 'L3MON4D3/LuaSnip' },
		},
	},
}
