local telescope = require 'telescope.builtin'
local map = vim.keymap.set

map('n', '<Leader>f'    , telescope.find_files, {})
map('n', '<Leader><Tab>', telescope. git_files, {})
