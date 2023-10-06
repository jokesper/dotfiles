local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
vim.opt.rtp:prepend(lazypath)
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system { 'git', 'clone',
		'--filter=blob:none',
		'https://github.com/folke/lazy.nvim.git',
		'--branch=stable',
		lazypath }
elseif #vim.api.nvim_list_uis() > 0 and os.getenv 'NVIM' ~= nil then
	require 'lazy'.setup 'jkeDev/plugins/flatten'
	return
end

vim.g.mapleader = ' '

require 'lazy'.setup 'jkeDev/plugins'
for _, name in ipairs {
	'vars', 'opts', 'keys', 'templates', 'autocmds',
} do if require(('jkeDev.%s'):format(name)) == false then return end end
vim.cmd.colorscheme 'jkeDev'
