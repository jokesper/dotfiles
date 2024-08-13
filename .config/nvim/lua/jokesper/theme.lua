local colors = require 'jokesper.colors'

local theme = {
	ColorColumn = { bg = 'red' },
	Conceal = { bg = 'gray' },
	--Cursor = {},
	--lCursor = {},
	--CursorIM = {},
	Directory = { fg = 'lightblue' },
	--TermCursor = {},
	--TermCursorNC = {},
	Folded = { bg = 3 },
	--FoldColumn = {},
	--SignColumn = {},
	--Substitute = {},
	CursorLine = { bold = true },
	CursorColumn = { bold = true },
	CursorLineNr = { fg = 2 },
	LineNr = { fg = 3 },
	--EndOfBuffer = { link = 'NonText' },
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
	PmenuSel = { fg = 'lightblue', bg = 4 },
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
	StatusLine = { fg = 'lightblue', bg = 2 },
	StatusLineNC = { fg = 3, bg = 2 },
	TabLineSel = { fg = 'lightblue', bg = 3 },
	TabLine = { fg = 3, bg = 3 },
	TabLineFill = { bg = 3 },
	WinSeparator = { fg = -2 },
	--Title = {},
	--Whitespace = {},
	--WildMenu = {},
	--WinBar = {},
	--WinBarNC = {},

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
	Function = { fg = 'lightblue' },

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
	Delimiter = { fg = 'lightblue' },
	Debug = { link = 'PreProc' },

	Underlined = { underline = true },

	Error = { fg = 'red', undercurl = true },

	ToDo = { underline = true },

	['@markup'] = { fg = 'lightblue' },
	['@markup.raw.markdown_inline'] = { fg = 'blue' },

	-- Diagnostics
	DiagnosticError = { fg = 'red' },
	DiagnosticWarn = { fg = 'yellow' },
	DiagnosticInfo = { fg = 'blue' },
	DiagnosticHint = { fg = 'green' },

	-- Treesitter
	['@text.literal'] = { fg = 'lightblue' },
	['@text.uri'] = { fg = 'blue', underline = true },

	-- Telescope
	TelescopeNormal = { link = 'Pmenu' },
	TelescopeSelection = { link = 'PmenuSel' },
	TelescopeMultiSelection = { fg = 'blue' },
	TelescopeBorder = { fg = 3, bg = 3 },
	TelescopePromptBorder = { fg = 'lightblue', bg = 3 },

	-- Indent Blankline
	IblIndent = { fg = 6 },
	IblScope = { fg = 4 },

	-- leap.nvim
	LeapLabelPrimary = { fg = 'orange' },
	LeapLabelSecondary = { fg = 'red' },
	LeapBackdrop = { underline = false },
}

for _, val in pairs(theme) do
	val.fg = colors.fg[val.fg]
	val.bg = colors.bg[val.bg]
end

return theme
