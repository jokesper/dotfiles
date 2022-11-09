local api = vim.api

api.nvim_create_augroup('templates', {clear = true})
for filetype, pattern in pairs({
    bash = '*.sh'
}) do
    api.nvim_create_autocmd(
        "BufNewFile",
        {
            group = 'templates',
            desc = string.format(
                "A template for %s files (%s).",
                filetype, tostring(pattern)),
            pattern = pattern,
            callback = require('jkeDev.templates.' .. filetype)
        })
end
