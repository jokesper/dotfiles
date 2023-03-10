local api = vim.api
local create_augroup = api.nvim_create_augroup
local create_autocmd = api.nvim_create_autocmd
local exec = api.nvim_exec
local buf_get_option = api.nvim_buf_get_option
local is_buffer_modifiable = function()
	local buf = api.nvim_get_current_buf()
	return buf_get_option(buf, 'modifiable')
		and not buf_get_option(buf, 'binary')
end

for name,augroup in pairs{custom = {
	{'TermOpen',
		desc = 'Automatically enter Terminal-mode when opening a new terminal window.',
		command = 'startinsert',
	},
	{'TermClose',
		pattern = 'term://*:*{bash}',
		desc = 'Automatically close terminal window when exiting shells.',
		command = 'quit!',
	},
	{'WinEnter',
		pattern = 'term://*',
		desc = 'Automatically enter terminal mode when entering a terminal window.',
		command = 'startinsert',
	},
	{'BufLeave',
		desc = 'Automatically leave insert mode when changing terminal window buffer.',
		command = 'stopinsert',
	},
	{'BufWritePre',
		-- FIXME: Disable on binary files
		desc = 'Automatically remove trailing whitespaces.',
		callback = function()
			if not is_buffer_modifiable() then return end
			for _,pattern in ipairs{
				[[\s\+$]], -- Remove trailing spaces
				[[\%^\n\+]], -- Remove leading newlines
				[[\n\+\%$]], -- Remove trailing newlines
			} do exec(('%%s/%s//e'):format(pattern), {silent=true}) end
		end,
	}},
} do
	if type(name) == 'string' then create_augroup(name, {clear = true}) end
	for _,autocmd in ipairs(augroup) do
		event, opts = {}, {group = name}
		for k,v in pairs(autocmd)
			do (type(k) == 'number' and event or opts)[k] = v end
		create_autocmd(event, opts)
	end
end
