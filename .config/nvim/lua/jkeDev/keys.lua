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

for lhs,rhs in pairs{
	['<A-h>'] = '<C-w>h', ['<A-H>'] = '<C-w>H',
	['<A-j>'] = '<C-w>j', ['<A-J>'] = '<C-w>J',
	['<A-k>'] = '<C-w>k', ['<A-K>'] = '<C-w>K',
	['<A-l>'] = '<C-w>l', ['<A-L>'] = '<C-w>L',
	['<A-n>'] = '<Cmd>new<CR>', ['<A-m>'] = '<Cmd>vnew<CR>',
	['<A-t>'] = '<Cmd>vnew +terminal<CR>', ['<A-T>'] = '<Cmd>new +terminal<CR>',
	['<A-=>'] = '<C-w>=',
} do map('nvot!', lhs, '<Cmd>stopinsert<CR>'..rhs) end

map('i', 'jk', '<ESC>')
map('i', '<C-c>', '<ESC>')
map('n', '<Leader>x', '<Cmd>Ex<CR>')

-- System clipboard
map('nv', '<Leader>y', '"+y')
map('nv', '<Leader>Y', '"+y$')
map('nv', '<Leader>p', '"+p')
map('nv', '<Leader>P', '"+P')
map('nv', '<Leader>d', '"+d')
map('nv', '<Leader>D', '"+d$')
map('nv', '<Leader>c', '"+c')
map('nv', '<Leader>C', '"+c$')
map('v', '<Leader>x', '"+x')
map('v', '<Leader>X', '"+X')

local v, max = vim.v, math.max
map('v', 'J', function() return (":m '>+%i<CR>gv=gv"):format(max(1,v.count)) end, {expr = true})
map('v', 'K', function() return (":m '<-%i<CR>gv=gv"):format(1+max(1,v.count)) end, {expr = true})
