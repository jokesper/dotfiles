local map = vim.api.nvim_set_keymap

map('i', 'jk'       , '<ESC>'              , {}) -- leave insert mode
map('n', '<Leader>' , '<Cmd>nohlsearch<CR>', {silent=true})
map('n', '<Leader>x', '<Cmd>Ex<CR>'        , {silent=true})
