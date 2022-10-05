local map = vim.api.nvim_set_keymap

map('i', 'jk'     , '<ESC>'          , {}) -- leave insert mode
map('n', '<SPACE>', ':nohlsearch<CR>', {silent=true})
