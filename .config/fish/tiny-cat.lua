#!/usr/bin/env -S nvim --clean --headless +:so --

local bit = require 'bit'

local boxChars = {
	' ','🬀','🬁','🬂',
	'🬃','🬄','🬅','🬆',
	'🬇','🬈','🬉','🬊',
	'🬋','🬌','🬍','🬎',
	'🬏','🬐','🬑','🬒',
	'🬓','▌','🬔','🬕',
	'🬖','🬗','🬘','🬙',
	'🬚','🬛','🬜','🬝',
	'🬞','🬟','🬠','🬡',
	'🬢','🬣','🬤','🬥',
	'🬦','🬧','▐','🬨',
	'🬩','🬪','🬫','🬬',
	'🬭','🬮','🬯','🬰',
	'🬱','🬲','🬳','🬴',
	'🬵','🬶','🬷','🬸',
	'🬹','🬺','🬻','█',
}

local function writeFullLine(line)
	for _, cs in ipairs(line)
		do io.write(boxChars[cs+1]) end
	io.write '\n'
end

local files
do
	local cmdlineH = io.open '/proc/self/cmdline'
	assert(cmdlineH ~= nil, "What the fuck are you doing?!")
	local cmdline = cmdlineH:read 'a'
	cmdlineH:close()
	local i, k = cmdline:find('\0', ({cmdline:find('\0--\0', 1, true)})[2] + 1, true) + 1, 0
	function files()
		local j = cmdline:find('\0', i, true)
		if j == nil then return nil end
		k, i = i, j+1
		return cmdline:sub(k,j-1)
	end
end


-- NOTE: uses 0-based indexing
local i, res = 0, {}
for file in files do
	for line in io.lines(file) do
		for j = 0,#line-1 do
			local index = math.floor(j/2) + 1
			local char = line:sub(j+1,j+1)
			res[index] = bit.bor(
				bit.lshift(
					char:match '[%s,._\'(){}]' and 0 or 1,
					j % 2 + 2 * i),
				res[index] or 0)
		end
		i = i + 1
		if i >= 3 then
			writeFullLine(res)
			i, res = 0, {}
		end
	end
end
if res then writeFullLine(res) end

vim.cmd.qa()
