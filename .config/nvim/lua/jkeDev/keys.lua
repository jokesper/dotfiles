do
    local native = vim.keymap.set
    vim.keymap.set = function(modes, lhs, rhs, opts)
        if opts.silent == nil then opts.silent = true end
        if type(modes) ~= 'string' then native(modes, lhs, rhs, opts) return end
        for mode in modes:gmatch('.') do native(mode, lhs, rhs, opts) end
    end
end
local map = vim.keymap.set

map('n', '<C-D>', '<C-D>Mg0' , {})
map('n', '<C-U>', '<C-U>Mg0' , {})
map('n', 'n'    , 'nzz'      , {})
map('n', 'N'    , 'Nzz'      , {})

map('nvot!', '<A-h>', '<C-\\><C-N><C-W>h', {})
map('nvot!', '<A-j>', '<C-\\><C-N><C-W>j', {})
map('nvot!', '<A-k>', '<C-\\><C-N><C-W>k', {})
map('nvot!', '<A-l>', '<C-\\><C-N><C-W>l', {})

map('i', 'jk'       , '<ESC>'              , {}) -- leave insert mode
map('n', '<Leader>n', '<Cmd>nohlsearch<CR>', {})
map('n', '<Leader>x', '<Cmd>Ex<CR>'        , {})
