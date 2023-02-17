do
	local native = vim.keymap.set
	vim.keymap.set = function(modes, lhs, rhs, opts)
		opts = opts or {}
		if opts.silent == nil then opts.silent = true end
		if type(modes) ~= 'string' then native(modes, lhs, rhs, opts) return end
		for mode in modes:gmatch '.' do native(mode, lhs, rhs, opts) end
	end
end
local map = vim.keymap.set

map('n', '<C-d>', '<C-d>Mg0')
map('n', '<C-u>', '<C-u>Mg0')
map('n', '<C-e>', '<C-e>Mg0')
map('n', '<C-y>', '<C-y>Mg0')
map('n', 'n', 'nzz')
map('n', 'N', 'Nzz')

map('nvot!', '<A-h>', '<C-\\><C-n><C-w>h')
map('nvot!', '<A-j>', '<C-\\><C-n><C-w>j')
map('nvot!', '<A-k>', '<C-\\><C-n><C-w>k')
map('nvot!', '<A-l>', '<C-\\><C-n><C-w>l')

map('nvot!', '<A-H>', '<C-\\><C-n><C-w>H')
map('nvot!', '<A-J>', '<C-\\><C-n><C-w>J')
map('nvot!', '<A-K>', '<C-\\><C-n><C-w>K')
map('nvot!', '<A-L>', '<C-\\><C-n><C-w>L')

map('i', 'jk', '<ESC>')
map('n', '<Leader>x', '<Cmd>Ex<CR>')

-- System clipboard
map('nv', '<Leader>y', '"+y')
map('nv', '<Leader>Y', '"+Y')
map('nv', '<Leader>p', '"+p')
map('nv', '<Leader>P', '"+P')
map('nv', '<Leader>d', '"+d')
map('nv', '<Leader>D', '"+D')
map('v', '<Leader>x', '"+x')
map('v', '<Leader>X', '"+X')

map('v', 'J', ":m '>+1<CR>gv=gv")
map('v', 'K', ":m '<-2<CR>gv=gv")
