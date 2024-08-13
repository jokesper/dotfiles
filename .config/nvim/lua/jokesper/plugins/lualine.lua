local colors = require 'jokesper.colors'
local theme = require 'jokesper.theme'

local lualine_theme = {
	normal = {
		a = { fg = 3, bg = 4, gui = 'bold' },
		b = { fg = 3, bg = 2 },
		c = 'StatusLine',
	},
	insert = {
		a = { fg = -2, bg = '-green', gui = 'bold' },
	},
	replace = {
		a = { fg = -2, bg = '-orange', gui = 'bold' },
	},
	visual = {
		a = { fg = -2, bg = '-blue', gui = 'bold' },
	},
	command = {
		a = { fg = -2, bg = -1, gui = 'bold' },
	},
	inactive = {
		c = 'StatusLineNC',
	},
}

for _, section in pairs(lualine_theme) do
	for name, val in pairs(section) do
		if type(val) == 'table' then
			val.fg = colors.fg[val.fg]
			val.bg = colors.bg[val.bg]
		else
			section[name] = theme[val]
		end
	end
end

lualine_theme.terminal = lualine_theme.command

return {
	{
		'nvim-lualine/lualine.nvim',
		opts = {
			options = {
				theme = lualine_theme,
			},
		},
	},
}
