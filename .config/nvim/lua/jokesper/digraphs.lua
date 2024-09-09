for chars, digraph in pairs {
	['{{'] = '⦃',
	['}}'] = '⦄',
	['lu'] = '⊔',
	['TT'] = '⊤',
	['CC'] = 'ℂ',
	['NN'] = 'ℕ',
	['QQ'] = 'ℚ',
	['RR'] = 'ℝ',
	['ZZ'] = 'ℤ',
} do vim.fn.digraph_set(chars, digraph) end
