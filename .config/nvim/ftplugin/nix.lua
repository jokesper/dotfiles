local opts = {
	expandtab = true,
	tabstop = 2,
	shiftwidth = 2,
}

for k, v in pairs(opts) do vim.opt_local[k] = v end
-- Fixme:
-- see `:h undo_ftplugin`
