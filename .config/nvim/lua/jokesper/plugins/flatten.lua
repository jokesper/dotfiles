return { {
	'willothy/flatten.nvim',
	opts = {
		callbacks = {
			should_nest = function(...)
				return #vim.api.nvim_list_uis() <= 0 or require 'flatten'.default_should_nest(...)
			end,
		},
		window = { open = 'tab' },
	},
	lazy = false,
	priority = 1001,
} }
