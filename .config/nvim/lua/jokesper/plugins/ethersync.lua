return {
	{
		'ethersync/ethersync',
		config = function(plugin)
			local dir = plugin.dir .. '/vim-plugin'
			vim.opt.rtp:append(dir)
			require 'lazy.core.loader'.packadd(dir)
		end,
		keys = { { '<leader>j', '<cmd>EthersyncJumpToCursor<cr>' } },
		lazy = false,
	},
}
