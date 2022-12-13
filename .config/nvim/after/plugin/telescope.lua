local telescope = require 'telescope.builtin'
local map = map_key

map('n', '<Leader>f'    , telescope.find_files, {})
map('n', '<Leader><Tab>', telescope. git_files, {})
