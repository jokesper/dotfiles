local opt = vim.opt

-- FIXME:
-- 1. Scrolloff fraction is hard coded
vim.api.nvim_create_augroup('options', { clear = true })
vim.api.nvim_create_autocmd({ 'VimEnter', 'VimResume', 'VimResized', 'WinNew', 'WinClosed' }, {
	group = 'options',
	desc = 'Update scrolloff value',
	callback = function()
		for _, id in pairs(vim.api.nvim_list_wins()) do
			local wo = vim.wo[id]
			wo.scrolloff, wo.sidescrolloff =
				math.ceil(vim.api.nvim_win_get_height(id) / 10),
				math.ceil(vim.api.nvim_win_get_width(id) / 10)
		end
	end,
})

opt.title = true
opt.titlestring = '%<%F - nvim'

opt.linebreak = true
opt.breakindent = true

opt.termguicolors = true

opt.cursorline = true
opt.cursorlineopt = 'number'

opt.ignorecase = true
opt.smartcase = true

opt.tabstop = 4
opt.shiftwidth = 4
opt.smartindent = true

opt.splitright = true
opt.splitbelow = true

opt.updatetime = 250
opt.timeoutlen = 800
