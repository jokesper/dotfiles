-- https://github.com/folke/tokyonight.nvim/blob/main/lua/tokyonight/theme.lua

local theme = require 'jkeDev.colors'

for name, val in pairs {
	ColorColumn = { bg = 'red' },
	--Conceal = {},
	--Cursor = {},
	--lCursor = {},
	--CursorIM = {},
	Directory = { fg = 'cyan' },
	--TermCursor = {},
	--TermCursorNC = {},
	--Folded = {},
	FoldColumn = {},
	SignColumn = {},
	--Substitute = {},
	CursorLine = { bold = true },
	CursorColumn = { bold = true },
	CursorLineNr = { fg = 2 },
	LineNr = { fg = 3 },
	EndOfBuffer = { fg = 4 },
	--CursorLineSign = {},
	--CursorLineFold = {},
	--MsgArea = {},
	--MsgSeparator = {},
	--MoreMsg = {},
	Normal = { fg = 1 },
	Visual = { bg = 4 },
	SpecialKey = { link = 'Comment' },
	NonText = { fg = 4 },
	MatchParen = { bold = true },
	--VisualNOS = {},
	--NormalFloat = {},
	--NormalNC = {},
	Pmenu = { fg = 1, bg = 3 },
	PmenuSel = { fg = 'cyan', bg = 4 },
	--PmenuSbar = {},
	--PmenuThumb = {},
	--Question = {},
	--QuickFixLine = {},
	CurSearch = { fg = -3, bg = -3 },
	IncSearch = { link = 'CurSearch' },
	Search = { fg = -3, bg = -4 },
	--SpellBad = {},
	--SpellCap = {},
	--SpellLocal = {},
	--SpellRare = {},
	ModeMsg = { link = 'Normal' },
	StatusLine = { fg = 'cyan', bg = 2 },
	StatusLineNC = { fg = 3, bg = 2 },
	TabLineSel = { fg = 'cyan', bg = 3 },
	TabLine = { fg = 3, bg = 3 },
	TabLineFill = { bg = 3 },
	WinSeparator = { fg = -2 },
	--Title = {},
	--Whitespace = {},
	--WildMenu = {},
	--WinBar = {},
	--WinBarNC = {},
	--
	ErrorMsg = { fg = 'red' },
	WarningMsg = { fg = 'orange' },

	DiffAdd = { bg = 'green' },
	DiffChange = { bg = 'orange' },
	DiffDelete = { bg = 'red' },
	DiffText = { bg = 'orange', bold = true },

	-- :h group-name
	Comment = { fg = 5 },

	Constant = { fg = 'red' },
	String = { fg = 'green' },
	Character = { link = 'String' },
	Number = { fg = 'orange' },
	Boolean = { link = 'Number' },
	Float = { link = 'Number' },

	Identifier = { link = 'Normal' },
	Function = { fg = 'cyan' },

	Statement = { link = 'Normal' },
	Conditional = { link = 'Keyword' },
	Repeat = { link = 'Keyword' },
	Label = { link = 'Keyword' },
	Operator = { fg = 'orange' },
	Keyword = { fg = 'magenta' },
	Exception = { link = 'Keyword' },

	PreProc = { fg = 4 },

	Type = { fg = 'red' },

	Special = { fg = 'blue' },
	Delimiter = { fg = 'cyan' },
	Debug = { link = 'PreProc' },

	Underlined = { underline = true },

	Error = { fg = 'red', undercurl = true },

	ToDo = { underline = true },

	-- Treesitter

	-- Telescope
	TelescopeNormal = { link = 'Pmenu' },
	TelescopeSelection = { link = 'PmenuSel' },
	TelescopeMultiSelection = { fg = 'blue' },
	TelescopeBorder = { fg = 3, bg = 3 },
	TelescopePromptBorder = { fg = 'cyan', bg = 3 },

	-- Indent Blankline
	IblIndent = { fg = 5 },
	IblScope = { fg = 4 },
} do
	val.fg = theme.fg[val.fg] == nil and val.fg or theme.fg[val.fg]
	val.bg = theme.bg[val.bg] == nil and val.bg or theme.bg[val.bg]
	vim.api.nvim_set_hl(0, name, val)
end
