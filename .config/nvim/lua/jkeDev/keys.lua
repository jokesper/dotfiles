local function map(mode, lhs, rhs, opts)
    ({
        ['string'  ] = vim.api.nvim_set_keymap,
        ['function'] = vim.keymap.set
    })[type(rhs)](mode, lhs, rhs, opts)
end

local telescope = require 'telescope.builtin'

map('n', '<C-D>', '<C-D>Mg0' , {})
map('n', '<C-U>', '<C-U>Mg0' , {})
map('n', 'n'    , 'nzz'      , {})
map('n', 'N'    , 'Nzz'      , {})

--map('t', '<ESC>'    , '<C-\\><C-N>'        , {}) -- leave terimal mode
map('i', 'jk'       , '<ESC>'              , {}) -- leave insert mode
map('n', '<Leader>n', '<Cmd>nohlsearch<CR>', {silent=true})
map('n', '<Leader>x', '<Cmd>Ex<CR>'        , {silent=true})
map('n', '<Leader>f', telescope.find_files , {})
