local api = vim.api
local augroup = api.nvim_create_augroup
local autocmd = api.nvim_create_autocmd

augroup('custom', {clear = true})
autocmd('TermOpen', {
    group = 'custom',
    desc = "Automatically enter Terminal-mode when opening a terminal.",
    command = 'startinsert'
})
autocmd('TermClose', {
	group = 'custom',
	pattern = 'term://*:*{bash}',
	desc = 'Automatically close terminal window when exiting shells.',
	command = 'quit!',
})
autocmd('WinEnter', {
	group = 'custom',
	pattern = 'term://*',
	desc = 'Automatically enter Terminal-mode when entering a terminal window.',
	command = 'startinsert',
})
