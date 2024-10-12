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
				'dist-newstyle/',
			},
		},
	},
	init = function()
		local telescope = require 'telescope.builtin'
		for lhs, rhs in pairs {
			h = function()
				telescope.find_files {
					hidden = true,
				}
			end,
			g = function()
				telescope.git_files {
					use_git_root = false,
					show_untracked = true,
				}
			end,
			t = telescope.live_grep,
			n = telescope.quickfix,
		} do vim.keymap.set('n', '<Leader>' .. lhs, rhs, { silent = true }) end
	end,
} }
