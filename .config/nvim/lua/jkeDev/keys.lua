local map = vim.api.nvim_set_keymap
--local function map(modes, ...)
--    for mode in modes:gmatch'.' do
--        vim.api.nvim_set_keymap(mode, ...)
--    end
--end
map('n', '<C-D>', '<C-D>zzg0', {})
map('n', '<C-U>', '<C-U>zzg0', {})
map('n', 'n'    , 'nzz'      , {})
map('n', 'N'    , 'Nzz'      , {})

--map('t', '<ESC>'    , '<C-\\><C-N>'        , {}) -- leave terimal mode
map('i', 'jk'       , '<ESC>'              , {}) -- leave insert mode
map('n', '<Leader>n', '<Cmd>nohlsearch<CR>', {silent=true})
map('n', '<Leader>x', '<Cmd>Ex<CR>'        , {silent=true})
