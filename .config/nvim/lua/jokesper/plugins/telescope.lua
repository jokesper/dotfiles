return { {
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
		} do vim.keymap.set('n', '<Leader>' .. lhs, rhs, { silent = true }) end
	end,
} }
