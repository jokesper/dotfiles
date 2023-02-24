local g = vim.g
local env = vim.env

-- Some settings should always be there for netrw to function properly
-- see `:echo g:netrw_bufsettings` when in `nvim --clean`
g.netrw_bufsettings = 'noma nomod nu nobl nowrap ro rnu'
g.netrw_banner = 0
g.vimtex_compiler_latexmk = {
	build_dir = ('%s/Documents/Output'):format(env.HOME)
}
g.vimtex_indent_on_ampersands = 0
