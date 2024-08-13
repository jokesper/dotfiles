for name, val in pairs(require 'jokesper.theme') do vim.api.nvim_set_hl(0, name, val) end
