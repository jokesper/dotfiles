local opts = {
	expandtab = true,
	tabstop = 4,
	shiftwidth = 4,
}

for k, v in pairs(opts) do vim.opt_local[k] = v end
-- Fixme:
-- see `:h undo_ftplugin`
