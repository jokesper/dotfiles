local api = vim.api
local augroup, autocmd = api.nvim_create_augroup, api.nvim_create_autocmd

augroup('custom', {clear = true})
autocmd('TermOpen', {
    group = 'custom',
    desc = "Automatically enter Terminal-mode when opening a terminal.",
    command = 'startinsert'
})
