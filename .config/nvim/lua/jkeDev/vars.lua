local g   = vim.g
local env = vim.env

g.netrw_bufsettings = 'rnu nu'
g.netrw_banner = 0
g.vimtex_compiler_latexmk = {
    build_dir = ('%s/Documents/Output'):format(env.HOME)
}
