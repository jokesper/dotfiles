local opts = {
	expandtab = true,
}

for k, v in pairs(opts) do vim.opt_local[k] = v end
-- Fixme:
-- see `:h undo_ftplugin`
