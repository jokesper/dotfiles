local opt = vim.opt
local api = vim.api

opt.title          = true
opt.titlestring    = "%<%F - nvim"
opt.relativenumber = true
opt.number         = true
opt.whichwrap      = "lh"

-- FIXME:
-- 1. Scrolloff faction is hard coded
-- 2. O(n) instead of O(1) for n = number of windows when opening a new window
-- 3. For NeoVim < 0.8 nvim_create_autocmd is not supported
api.nvim_create_autocmd({"VimEnter","VimResume","VimResized","WinNew","WinClosed"}, {
    desc = "Update scrolloff value",
    callback = function()
        for _,id in pairs(vim.api.nvim_list_wins()) do
            vim.wo[id].scrolloff =
                math.ceil(api.nvim_win_get_height(id) / 10)
            vim.wo[id].sidescrolloff =
                math.ceil(api.nvim_win_get_width (id) / 10)
        end
    end})
-- Possible fix for the 2.:
-- api.nvim_create_autocmd({"WinNew", "WinScrolled"}, {
--    desc = "Update scrolloff value"
--    callback = function()
--        
--    end})

opt.encoding       = 'utf8'
opt.fileencoding   = 'utf8'
opt.linebreak      = true

opt.syntax         = "ON"
opt.termguicolors  = true
opt.spelllang      = "en"

opt.foldmethod     = "indent"
opt.foldenable     = false

opt.ignorecase     = true
opt.smartcase      = true
opt.incsearch      = true
opt.hlsearch       = true

opt.expandtab      = true
opt.shiftwidth     = 4
opt.shiftround     = true
opt.smarttab       = true
opt.softtabstop    = 4
opt.tabstop        = 4
opt.autoindent     = true
opt.smartindent    = true

opt.splitright     = true
opt.splitbelow     = true

--opt.lazyredraw = true
