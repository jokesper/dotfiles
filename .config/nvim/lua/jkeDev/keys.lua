do
    local native = vim.keymap.set
    vim.keymap.set = function(modes, ...)
        if type(modes) ~= 'string' then native(modes, ...) return end
        for mode in modes:gmatch('.') do native(mode, ...) end
    end
end
local map = vim.keymap.set

map('n', '<C-D>', '<C-D>Mg0' , {})
map('n', '<C-U>', '<C-U>Mg0' , {})
map('n', 'n'    , 'nzz'      , {})
map('n', 'N'    , 'Nzz'      , {})

map('nvot!', '<A-h>', '<C-\\><C-N><C-W>h', {silent=true})
map('nvot!', '<A-j>', '<C-\\><C-N><C-W>j', {silent=true})
map('nvot!', '<A-k>', '<C-\\><C-N><C-W>k', {silent=true})
map('nvot!', '<A-l>', '<C-\\><C-N><C-W>l', {silent=true})

--map('t', '<ESC>'    , '<C-\\><C-N>'        , {}) -- leave terimal mode
map('i', 'jk'       , '<ESC>'              , {}) -- leave insert mode
map('n', '<Leader>n', '<Cmd>nohlsearch<CR>', {silent=true})
map('n', '<Leader>x', '<Cmd>Ex<CR>'        , {silent=true})
