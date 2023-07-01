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
	sources = {
		{name = 'luasnip'},
		{name = 'calc'},
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
