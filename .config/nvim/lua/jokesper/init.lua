local lazypath = vim.fn.stdpath 'data' .. '/lazy/lazy.nvim'
vim.opt.rtp:prepend(lazypath)
if not vim.loop.fs_stat(lazypath) then
	vim.fn.system { 'git', 'clone',
		'--filter=blob:none',
		'https://github.com/folke/lazy.nvim.git',
		'--branch=stable',
		lazypath }
elseif #vim.api.nvim_list_uis() > 0 and os.getenv 'NVIM' ~= nil then
	require 'lazy'.setup 'jokesper/plugins/flatten'
	return
end

vim.g.mapleader = ' '
vim.g.maplocalleader = ' '

-- I don't use netrw anymore but I still keep those options set
-- Some settings should always be there for netrw to function properly
-- see `:echo g:netrw_bufsettings` when in `nvim --clean`
vim.g.netrw_bufsettings = 'noma nomod nu nobl nowrap ro rnu'
vim.g.netrw_banner = 0

vim.g.vimtex_compiler_latexmk = {
	out_dir = ('%s/Documents/Output'):format(vim.env.HOME),
}
vim.g.vimtex_compiler_latexmk_engines = {
	_ = '-lualatex',
}
vim.g.vimtex_indent_on_ampersands = 0

vim.g.firenvim_config = {
	localSettings = {
		['.*'] = {
			takeover = 'never',
		},
	},
}

for _, name in ipairs {
	'opts', 'keys', 'templates', 'autocmds',
} do if require(('jokesper.%s'):format(name)) == false then return end end
vim.cmd.colorscheme 'jokesper'

require 'lazy'.setup {
	spec = {
		{ import = 'jokesper/plugins' },
	},
	install = {
		colorscheme = { 'jokesper' },
	},
	headless = {
		task = false,
	},
	change_detection = {
		notify = false,
	},
}
