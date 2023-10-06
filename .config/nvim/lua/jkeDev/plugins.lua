local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
vim.opt.rtp:prepend(lazypath)
local should_nest = #vim.api.nvim_list_uis() <= 0
local flatten = {
	'willothy/flatten.nvim',
	opts = {
		callbacks = {
			should_nest = function(...)
				return should_nest or require 'flatten'.default_should_nest(...)
			end,
		},
		window = { open = 'tab' },
	},
	lazy = false,
	priority = 1001,
}
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system { 'git', 'clone',
		'--filter=blob:none',
		'https://github.com/folke/lazy.nvim.git',
		'--branch=stable',
		lazypath }
elseif not should_nest and os.getenv 'NVIM' ~= nil then
	require 'lazy'.setup { flatten }
	return false
end

local tty = vim.fn.getenv 'TERM' == 'linux'
require 'lazy'.setup {
	flatten,
	{
		'nvim-treesitter/nvim-treesitter',
		build = function() require 'nvim-treesitter.install'.update { with_sync = true } () end,
	},
	{
		'nvim-treesitter/nvim-treesitter-context',
		main = 'nvim-treesitter.configs',
		opts = {
			ensure_installed = {
				'vimdoc',
				'gitignore', 'gitcommit',
				'diff',
				'fish', 'bash', 'awk',
				'lua', 'rust', 'haskell',
				'markdown', 'latex',
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
	{ 'lervag/vimtex', ft = 'tex', lazy = true },
	{
		'nvim-telescope/telescope.nvim',
		branch = '0.1.x',
		dependencies = 'nvim-lua/plenary.nvim',
		opts = {
			defaults = {
				borderchars = vim.fn.getenv 'TERM' == 'linux'
					and { '─', '│', '─', '│', '+', '+', '+', '+' } or nil,
				file_ignore_patterns = {
					'Output/',
					'bin/',
					'obj/',
				},
			},
		},
		init = function()
			local telescope = require 'telescope.builtin'
			for lhs, rhs in pairs {
				f = telescope.find_files,
				['<Tab>'] = telescope.git_files,
				s = telescope.live_grep,
			} do vim.keymap.set('n', '<Leader>' .. lhs, rhs) end
		end,
	},
	{
		'glacambre/firenvim',
		cond = vim.g.started_by_firenvim == true,
		build = function()
			require 'lazy'.load { plugins = 'firenvim', wait = true }
			vim.fn['firenvim#install'](0)
		end
	},
	{
		'lukas-reineke/indent-blankline.nvim',
		main = 'ibl',
		opts = {
			indent = {
				char = vim.fn.getenv 'TERM' == 'linux' and '│' or nil,
			},
			scope = {
				include = {
					node_type = {
						lua = { 'table_constructor' },
					},
				},
			},
		},
	},
	{ 'nacro90/numb.nvim', config = true },
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
		branch = 'v3.x',
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
	{
		'folke/todo-comments.nvim',
		opts = {
			merge_keywords = false,
			keywords = { -- NOTE: non tty icons are from nerd fonts
				NOTE = { icon = tty and 'N' or '󰍩', color = 'hint', alt = { 'INFO' } },
				TODO = { icon = tty and 'T' or '', color = 'info' },
				FIX = { icon = tty and 'F' or 'F', color = 'error', alt = { 'FIXME' } },
				WARN = { icon = tty and 'W' or '', color = 'warning', alt = { 'WARNING' } },
				HACK = { icon = tty and 'H' or '', color = 'warning' },
				PERF = { icon = tty and 'P' or '󰁫', alt = { 'PERFORMANCE', 'OPTIMIZE' } },
			},
		},
	},
}
