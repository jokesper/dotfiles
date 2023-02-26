local fn, cmd = vim.fn, vim.cmd
local bootstrap = (function()
	local install_path = fn.stdpath 'data' .. '/site/pack/packer/start/packer.nvim'
	if fn.empty(fn.glob(install_path)) > 0 then
		fn.system{'git', 'clone', '--depth', '1', 'https://github.com/wbthomason/packer.nvim', install_path}
		cmd.packadd 'packer.nvim'
		return true
	else return false end
end)()

return require 'packer'.startup(function(use)
	use 'wbthomason/packer.nvim'
	use 'folke/tokyonight.nvim'
	use {
		'nvim-treesitter/nvim-treesitter',
		run = ':TSUpdate',
	}
	use {
		'lervag/vimtex',
		opt = true,
		event = 'BufWinEnter *.tex',
	}
	use {
		'nvim-telescope/telescope.nvim',
		branch = '0.1.x',
		requires = 'nvim-lua/plenary.nvim',
	}
	use {
		'glacambre/firenvim',
		opt = true,
		run = function() fn['firenvim#install'](0) end,
		setup = [[vim.cmd.packadd 'firenvim']],
	}
	use 'lukas-reineke/indent-blankline.nvim'

	if bootstrap then require('packer').sync() end
end)
