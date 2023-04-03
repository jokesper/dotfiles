local function negative(tbl, neg)
	return setmetatable(tbl, {__index = function(_,key)
		return rawget(neg, type(key) == 'number' and -key or key)
	end})
end
local fg = {
	'#bfbfbf',
	'#ffbfff',
	'#bf7fbf',
	'#7f3f7f',
	'#5f5f7f',

	blue = '#5f7fff',
	cyan = '#7fbfff',
	red = '#bf3f3f',
	green = '#7fbf7f',
	orange = '#ffbf7f',
	magenta = '#bf7fff',
}
local bg = {
	'',
	'#1f0f1f',
	'#171727',
	'#1f0f3f',

	green = '#1f3f1f',
	orange = '#7f3f0f',
	red = '#3f0f0f',
}

return {
	fg = negative(fg, bg),
	bg = negative(bg, fg),
}
