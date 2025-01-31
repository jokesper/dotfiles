return {
	{
		'VonHeikemen/lsp-zero.nvim',
		branch = 'v4.x',
		lazy = true,
	},
	{
		'williamboman/mason.nvim',
		config = true,
		build = function() pcall(vim.cmd.MasonUpdate) end,
	},
	{
		'hrsh7th/nvim-cmp',
		event = { 'InsertEnter', 'CmdLineEnter' },
		dependencies = {
			{ 'hrsh7th/cmp-cmdline' },
			{ 'hrsh7th/cmp-buffer' },
			{ 'hrsh7th/cmp-calc' },
			{ 'hrsh7th/cmp-path' },
			{ 'hrsh7th/cmp-nvim-lua' },
			{ 'L3MON4D3/LuaSnip', version = 'v2.*' },
			{ 'saadparwaiz1/cmp_luasnip' },
		},
		config = function()
			local lsp_zero, cmp = require 'lsp-zero', require 'cmp'
			local cmp_action = lsp_zero.cmp_action()
			cmp.setup {
				preselect = cmp.PreselectMode.None,
				mapping = { -- NOTE: don't use preset
					['<C-f>'] = cmp_action.luasnip_jump_forward(),
					['<C-b>'] = cmp_action.luasnip_jump_backward(),
					['<Tab>'] = cmp_action.luasnip_supertab(),
					['<S-Tab>'] = cmp_action.luasnip_shift_supertab(),
				},
				formatting = {
					fields = { 'abbr', 'kind', 'menu' },
					format = function(entry, item)
						item.menu = ({
							luasnip = '□',
							calc = '∑',
							async_path = '/',
							nvim_lua = '∃',
							nvim_lsp = '∃',
							buffer = '~',
							cmdline = ':',
						})[entry.source.name]
						item.kind = ({
							-- NOTE: NF = Nerd Fonts
							-- TODO: find fitting symbols for long names
							Method = 'Method',
							Function = 'λ',
							Constructor = 'Constructor',
							Field = '', -- NF
							Variable = '𝕏',
							Class = '⑆',
							Interface = '∩',
							Module = '', -- NF
							Property = '', -- NF
							Unit = '£',
							Value = '1',
							Enum = '∪',
							Keyword = 'a',
							Snippet = '□',
							Color = '#',
							File = '', -- NF
							Reference = '§',
							Folder = '', -- NF
							EnumMember = '∈',
							Constant = 'π',
							Struct = '×',
							Event = 'e',
							Operator = '±',
							TypeParameter = '<T>', -- TODO: replace with single charater
						})[require 'cmp.types.lsp'.CompletionItemKind[entry:get_kind()]]
						return item
					end,
				},
				sources = {
					{ name = 'luasnip' },
					{ name = 'calc' },
					{ name = 'path' },
					{ name = 'nvim_lua' },
					{ name = 'nvim_lsp', group_index = 1 },
					{ name = 'buffer', group_index = 2, keyword_length = 3 },
				},
				snippet = {
					expand = function(args) require 'luasnip'.lsp_expand(args.body) end,
				},
				window = {
					completion = {
						col_offset = -8,
					},
				},
				performance = {
					max_view_entries = 12,
				},
				experimental = {
					ghost_text = true,
				},
			}
			cmp.setup.cmdline('/', {
				mapping = cmp.mapping.preset.cmdline(),
				sources = {
					{ name = 'buffer' },
				},
			})
			cmp.setup.cmdline(':', {
				mapping = cmp.mapping.preset.cmdline(),
				sources = {
					{ name = 'async_path' },
					{ name = 'cmdline' },
				},
			})
		end,
	},
	{
		'neovim/nvim-lspconfig',
		cmd = { 'LspInfo', 'LspInstall', 'LspStart' },
		event = { 'BufReadPre', 'BufNewFile' },
		dependencies = {
			{ 'hrsh7th/cmp-nvim-lsp' },
			{ 'williamboman/mason-lspconfig.nvim' },
		},
		config = function()
			local lsp_zero = require 'lsp-zero'
			lsp_zero.extend_lspconfig {
				capabilities = require 'cmp_nvim_lsp'.default_capabilities(),
				lsp_attach = function(client, bufnr)
					lsp_zero.default_keymaps { buffer = bufnr }
					--lsp_zero.buffer_autoformat(client, bufnr)
					vim.keymap.set('n', 'gl', function() vim.diagnostic.open_float {} end, { buffer = bufnr })
				end,
			}
			require 'mason-lspconfig'.setup {
				ensure_installed = {
					'lua_ls',
				},
				handlers = {
					function(server) require 'lspconfig'[server].setup {} end,
					lua_ls = function()
						require 'lspconfig'.lua_ls.setup(lsp_zero.nvim_lua_ls {
							settings = {
								Lua = {
									format = {
										enable = true,
										defaultConfig = {
											end_of_line = 'lf',
											table_seperator_style = 'comma',
											trailing_table_seperator = 'smart',
											call_arg_parentheses = 'remove',
											quote_style = 'single',
											align_function_params = 'false',
											align_continuous_assign_statement = 'false',
											align_continuous_rect_table_field = 'false',
											align_continuous_line_space = '0',
											align_array_table = 'none',
											align_continuous_inline_comment = 'false',
										},
									},
								},
							},
						})
					end,
				},
			}
			local lspconfig = require 'lspconfig'
			lspconfig.texlab.setup {}
			lspconfig.rust_analyzer.setup {}
			lspconfig.clangd.setup {}
			lspconfig.hls.setup {
				settings = {
					haskell = {
						formattingProvider = 'fourmolu',
					},
				},
				filetypes = { 'haskell', 'lhaskell', 'cabal' },
			}
		end,
	},
}
