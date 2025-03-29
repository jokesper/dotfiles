local is_buffer_modifiable = function()
	local buf = vim.api.nvim_get_current_buf()
	return vim.api.nvim_buf_get_option(buf, 'modifiable')
		and not vim.api.nvim_buf_get_option(buf, 'binary')
end

local function setEnabledOfOpts(vars, opts, enable, opt, val, prefix)
	---@diagnostic disable-next-line
	if type(opt) == 'number' then opt, val = val, { false, true } end
	---@cast opt string
	---@cast val any[]
	val = { off = val[1], on = val[2] }
	local old = prefix .. opt
	if not enable then
		vars[old], opts[opt] = opts[opt], val.off
	elseif vars[old] ~= nil then
		opts[opt] = vars[old]
	else
		opts[opt] = val.on
	end
end
local function setOptionsOnCondition(win, prefix, enable)
	local vars, opts
	if win ~= nil then
		vars, opts = vim.w[win], vim.wo[win]
	else
		vars, opts = vim.w, vim.wo
	end
	for opt, val in pairs {
		'number', 'relativenumber',
		fillchars = { 'eob: ', '' },
		signcolumn = { 'no', 'yes' },
		foldcolumn = { '0', 'auto' },
		--colorcolumn = {'', '101'},
	} do setEnabledOfOpts(vars, opts, enable, opt, val, prefix) end
end
local function setOptionsOnFocus(enable)
	for _, win in ipairs(vim.api.nvim_list_wins()) do
		setOptionsOnCondition(win, 'when_focused_', enable)
	end
	for opt, val in pairs {
		showtabline = { 0, 1 },
		cmdheight = { 0, 1 },
	} do setEnabledOfOpts(vim.g, vim.o, enable, opt, val, 'when_focused_') end
end

for name, augroup in pairs { custom = {
	{
		'TermEnter',
		'TermLeave',
		desc = 'Hide linenumbers when entering terminal mode',
		callback = function(event)
			setOptionsOnCondition(nil, 'when_not_in_term_mode_', event.event ~= 'TermEnter')
		end,
	},
	{
		'TermOpen',
		desc = 'Automatically configure new terminal windows',
		callback = function()
			local opt = vim.opt_local
			opt.bufhidden = 'delete'
			vim.cmd 'startinsert'
		end,
	},
	-- FIXME: not triggered when closing windows and switching to old
	{
		'WinEnter',
		pattern = 'term://*',
		desc = 'Automatically enter terminal mode when entering a terminal window.',
		command = 'startinsert',
	},
	{
		'BufLeave',
		desc = 'Automatically leave insert mode when changing terminal window buffer.',
		command = 'stopinsert',
	},
	{
		'BufWritePre',
		-- FIXME: Disable on binary files
		desc = 'Automatically remove trailing whitespaces.',
		callback = function()
			if not is_buffer_modifiable() then return end
			for _, pattern in ipairs {
				[[ \+$]], -- Remove trailing spaces
				[[\%^\n\+]], -- Remove leading newlines
				[[\n\+\%$]], -- Remove trailing newlines
			} do vim.api.nvim_exec2(('%%s/%s//e'):format(pattern), {}) end
		end,
	},
	{
		'FocusGained',
		desc = 'Show line numbers when focused',
		callback = function() setOptionsOnFocus(true) end,
	},
	{
		'FocusLost',
		desc = 'Hide line numbers when not focused',
		callback = function() setOptionsOnFocus(false) end,
	},
} } do
	if type(name) == 'string' then vim.api.nvim_create_augroup(name, {}) end
	for _, autocmd in ipairs(augroup) do
		local event, opts = {}, { group = name }
		for k, v in pairs(autocmd)
		do (type(k) == 'number' and event or opts)[k] = v end
		vim.api.nvim_create_autocmd(event, opts)
	end
end

-- Set inital options. We don't use VimEnter,
-- since it's after commands run with `-c`
setOptionsOnFocus(true)
