local g   = vim.g
local env = vim.env

g.vimtex_compiler_latexmk = {
    build_dir = ('%s/Documents/Output'):format(env.HOME)
}
