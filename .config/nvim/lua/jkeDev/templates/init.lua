local api = vim.api

api.nvim_create_augroup('templates', {clear = true})
local function loader(template, ...)
    local indent = #template:match('^%s*')
    local cnt = {}
    for line in (template..'\n'):format(...):gmatch('([^\n]*)\n') do
        table.insert(cnt, line:sub(indent+1))
    end
    api.nvim_buf_set_lines(0, 0, -1, true, cnt)
    api.nvim_win_set_cursor(0, {#cnt, #cnt[#cnt]})
end
for _,format in ipairs({
    {
        function(loader, _) loader([[
            #!/bin/bash
            ]])
        end, '*.sh'
    }
}) do
    local template = table.remove(format, 1)
    if type(template) == 'string'
    then template = require('jkeDev.templates.' .. template) end

    api.nvim_create_autocmd(
        "BufNewFile",
        {
            group = 'templates',
            desc = "A template for files with extensions: " .. tostring(format),
            pattern = format,
            callback = function(event) template(loader, event) end
        })
end
