local opt = vim.opt
local api = vim.api

opt.title = true
opt.titlestring = '%<%F - nvim'

-- FIXME:
-- 1. Scrolloff faction is hard coded
-- 2. For NeoVim < 0.8 nvim_create_autocmd is not supported
api.nvim_create_augroup('options', { clear = true })
api.nvim_create_autocmd({ 'VimEnter', 'VimResume', 'VimResized', 'WinNew', 'WinClosed' }, {
	group = 'options',
	desc = 'Update scrolloff value',
	callback = function()
		for _, id in pairs(api.nvim_list_wins()) do
			local wo = vim.wo[id]
			wo.scrolloff, wo.sidescrolloff =
				math.ceil(api.nvim_win_get_height(id) / 10),
				math.ceil(api.nvim_win_get_width(id) / 10)
		end
	end,
})

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
