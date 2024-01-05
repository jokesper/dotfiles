return function(loader, event)
	loader([[
	\documentclass{article}

	\usepackage{amsmath}
	\usepackage{isotope}
	\usepackage{mathtools}
	\usepackage{amssymb}
	\usepackage{fdsymbol}
	\usepackage{slashed}
	\usepackage{geometry}
	\usepackage[shortlabels]{enumitem}
	\usepackage{graphicx}
	\usepackage{amsfonts}
	\usepackage[exponent-product = \cdot]{siunitx}
	\usepackage{pgfplots}
	\usepackage{pgfplotstable}
	\usepackage[autostyle]{csquotes}
	\usepackage{babel}
	\usepackage[document]{ragged2e}
	\usepackage[skip=1em]{parskip}

	\usetikzlibrary{angles,quotes}

	\pgfplotsset{compat = newest}

	\newcommand{\textop}[1]{\relax\ifmmode\mathop{\text{#1}}\else\text{#1}\fi}

	\geometry{margin=2cm}

	\title{%s}
	\author{%s}
	\date{%s}

	\begin{document}
		%^\maketitle
	\end{document}]],
		event.match:match '([^/]+)%.[^%.]+$',
		vim.split(vim.fn.system { 'getent', 'passwd', vim.env.USER }, ':')[5],
		os.date '%d.%m.%Y')
end
