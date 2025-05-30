local tty = vim.fn.getenv 'TERM' == 'linux'
return {
	{
		'nacro90/numb.nvim',
		opts = {
			hide_relativenumbers = false,
		},
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
			indent = { char = tty and '│' or nil },
			scope = {
				include = {
					node_type = {
						lua = { 'table_constructor' },
					},
				},
			},
			exclude = {
				filetypes = { 'oil' },
			},
		},
	},
	{
		'laytan/cloak.nvim',
		opts = {
			patterns = {
				{
					file_pattern = '*wpa_supplicant*.conf',
					cloak_pattern = { '(psk=).+', '(password=")[^"]+' },
					replace = '%1',
				},
				{
					file_pattern = '*/glab-cli/config.yml',
					cloak_pattern = { '(token:%s+)%w+', },
					replace = '%1',
				},
				{
					file_pattern = '*fstab',
					cloak_pattern = { '(username=)[^,%s]+', '(workgroup=)[^,%s]+', '(password=)[^,%s]+', '([ug]id=)[^,%s]+' },
					replace = '%1',
				},
			},
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
	{
		'ggandor/leap.nvim',
		config = function(plugin, opts)
			require 'leap'.add_default_mappings()
		end,
	},
	{
		'stevearc/oil.nvim',
		opts = {
			skip_confirm_for_simple_edits = true,
		},
	},
	{
		'jokesper/align.nvim',
		enabled = vim.fn.has 'nvim-0.10',
		opts = {
			align = {
				['*'] = {
					' = ',
				},
				typst = {
					'\\$',
				},
				haskell = {
					' :: ',
					' = ',
				},
				yaml = {
					': ',
				},
			},
		},
	},
}
