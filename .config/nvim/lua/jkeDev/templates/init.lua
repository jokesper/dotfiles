local api = vim.api

api.nvim_create_augroup('templates', {clear = true})
local function loader(template, ...)
    local cnt, indent = {}, template:match('^%s*')
    for line in (template:sub(#indent+1) .. '\n')
            :gsub('\n' .. indent, '\n')
            :format(...)
            :gmatch('([^\n]*)\n') do
        table.insert(cnt, line)
    end
    api.nvim_buf_set_lines(0, 0, -1, true, cnt)
    api.nvim_win_set_cursor(0, {#cnt, #cnt[#cnt]})
end
local function generate(pattern, template)
    return {pattern, function(loader, _) loader(template) end}
end

for _,format in ipairs({
    generate('*.sh', '#!/bin/bash\n'),
    {'*.tex', 'latex'}
}) do
    local template = table.remove(format)
    local callback
    if type(template) == 'string' then
        template = ('jkeDev.templates.%s'):format(template)
        callback = function(e) require(template)(loader, e) end
    else callback = function(e) template(loader, e) end end

    api.nvim_create_autocmd(
        "BufNewFile",
        {
            group = 'templates',
            desc = "A template for files with extensions: " .. tostring(format),
            pattern = format, callback = callback
        })
end
