local lsp = require 'lsp-zero'.preset{}

lsp.on_attach(function(client, bufnr)
	lsp.default_keymaps{buffer = bufnr}
end)

lsp.setup()

local cmp = require 'cmp'
local cmp_action = require 'lsp-zero'.cmp_action()
cmp.setup{
	mapping = {
		['<C-f>'] = cmp_action.luasnip_jump_forward(),
		['<C-b>'] = cmp_action.luasnip_jump_backward(),
		['<Tab>'] = cmp_action.luasnip_supertab(),
		['<S-Tab>'] = cmp_action.luasnip_shift_supertab(),
	},
	formatting = {
		fields = {'abbr', 'kind', 'menu'},
		format = function(entry, item)
			item.menu = ({
				luasnip = '□',
				calc = '∑',
				async_path = '/',
				nvim_lsp = '∃',
				buffer = '~',
				cmdline = ':',
			})[entry.source.name]
			item.kind = setmetatable({
				-- ToDo: find fitting symbols for long names
				Text = 'a',
				Method = 'Method',
				Function = 'λ',
				Constructor = 'Constructor',
				Field = 'Field',
				Variable = '𝕏',
				Class = 'Class',
				Interface = 'Interface',
				Module = 'Module',
				Property = 'Property',
				Unit = '$', -- ToDo: find a better currency or some other symbol
				Value = '1',
				Enum = 'Enum',
				Keyword = 'Keyword',
				Snippet = '□',
				Color = '#',
				File = 'File',
				Reference = '§',
				Folder = 'Folder',
				EnumMember = '∈',
				Constant = 'π',
				Struct = '∪',
				Event = 'e',
				Operator = '×',
				TypeParameter = '<T>', -- ToDo: replace with single charater
			}, {__index = function(_,type) return type..'?!' end})
				[require 'cmp.types.lsp'.CompletionItemKind[entry:get_kind()]]
			return item
		end,
	},
	sources = {
		{name = 'luasnip'},
		{name = 'calc'},
		{name = 'async_path'},
		{name = 'nvim_lsp', group_index = 1},
		{name = 'buffer', group_index = 2, keyword_length = 3},
	},
	window = {
		completion = {
			col_offset = -8,
		},
	},
	performance = {
		max_view_entries = 12,
	},
}
cmp.setup.cmdline('/', {
	mapping = cmp.mapping.preset.cmdline(),
	sources = {
		{name = 'buffer'},
	},
})
cmp.setup.cmdline(':', {
	mapping = cmp.mapping.preset.cmdline(),
	sources = {
		{name = 'async_path'},
		{name = 'cmdline'},
	},
})
