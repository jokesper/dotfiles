local telescope = require 'telescope.builtin'
local map = vim.keymap.set

require 'telescope'.setup {
	defaults = {file_ignore_patterns = {'Output/'}},
}

map('n', '<Leader>f', telescope.find_files)
map('n', '<Leader><Tab>', telescope.git_files)
map('n', '<Leader>s', telescope.live_grep)
