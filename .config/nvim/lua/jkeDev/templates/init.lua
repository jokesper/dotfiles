local api = vim.api
local augroup = api.nvim_create_augroup
local autocmd = api.nvim_create_autocmd

augroup('templates', {clear = true})
local function mapVararg(f, ...)
	local res = {}
	for k,v in pairs{...} do res[k] = f(v) end
	return unpack(res) -- moved to table.unpack
end
local function loader(template, ...)
	local cnt, cursor, indent = {}, {0}, template:match '^%s*'
	for line in (template:sub(#indent+1) .. '\n')
		:gsub('\n' .. indent, '\n')
		:gsub('(%%%%)', '%1%1')
		:gsub('%%+^', function(str)
			return #str % 2 == 0 and '%' .. str or str
		end)
		:format(mapVararg(function(v)
			return type(v) == 'string' and v:gsub('(%%+)', '%1%1') or v
		end, ...))
		:gmatch '([^\n]*)\n' do
		local i,s = 1, '_'
		repeat
			if #s % 2 == 0 then break end
			_,i,s = line:find('(%%+^)', i)
		until i == nil
		local preCursor = i ~= nil
		local line = line:gsub('%%(.)', function(c)
			if preCursor then i = i - (c == '%' and 1 or 2) end
			if c == '^' then preCursor = false end
			return c == '^' and '' or '%'
		end)
		if cursor[2] == nil then
			if i ~= nil then cursor[2] = i end
			cursor[1] = cursor[1] + 1
		end
		table.insert(cnt, line)
	end
	api.nvim_buf_set_lines(0, 0, -1, true, cnt)
	if cursor[2] == nil then cursor[2] = #cnt[#cnt] end
	api.nvim_win_set_cursor(0, cursor)
end
local function generate(pattern, template)
	return {pattern, function(loader, _) loader(template) end}
end

for _,format in ipairs({
	generate('*.sh', '#!/usr/bin/env bash\n'),
	{'*.tex', 'latex'},
}) do
	local template = table.remove(format)
	local callback
	if type(template) == 'string' then
		template = ('jkeDev.templates.%s'):format(template)
		callback = function(e) require(template)(loader, e) end
	else callback = function(e) template(loader, e) end end

	autocmd(
	'BufNewFile',
	{
		group = 'templates',
		desc = ('A template for files with extensions: %s'):format(tostring(format)),
		pattern = format, callback = callback,
	})
end
