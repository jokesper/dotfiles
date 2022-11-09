return function()
    vim.api.nvim_buf_set_lines(
        0,
        0, -1,
        true,
        {"#!/bin/bash",""})
    vim.api.nvim_win_set_cursor(0, {2,0})
end
