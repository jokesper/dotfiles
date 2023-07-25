local telescope = require 'telescope.builtin'
local actions = require 'telescope.actions'
local map = vim.keymap.set

require 'telescope'.setup {
	defaults = {
		file_ignore_patterns = {
			'Output/',
			'bin/',
			'obj/',
		},
		mappings = {
			i = {
				['<C-c>'] = false,
				['<esc>'] = actions.close,
			},
			n = { ['<C-c>'] = actions.close },
		},
	},
}

map('n', '<Leader>f', telescope.find_files)
map('n', '<Leader><Tab>', telescope.git_files)
map('n', '<Leader>s', telescope.live_grep)
