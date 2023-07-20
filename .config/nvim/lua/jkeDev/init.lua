vim.g.mapleader = ' '

for _,name in ipairs{
	'vars', 'opts', 'keys', 'templates', 'plugins', 'autocmds'
} do require(('jkeDev.%s'):format(name)) end
vim.cmd.colorscheme 'jkeDev'
