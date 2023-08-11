vim.g.mapleader = ' '

for _, name in ipairs {
	'plugins', 'vars', 'opts', 'keys', 'templates', 'autocmds'
} do if require(('jkeDev.%s'):format(name)) == false then return end end
vim.cmd.colorscheme 'jkeDev'
