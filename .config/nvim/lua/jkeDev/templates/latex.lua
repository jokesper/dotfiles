local sys = vim.fn.system
local env = vim.env
return function(loader, event) loader([[
	\documentclass{article}

	\usepackage{amsmath}
	\usepackage{amssymb}
	\usepackage{geometry}
	\usepackage[shortlabels]{enumitem}
	\usepackage{graphicx}
	\usepackage{amsfonts}
	\usepackage{units}

	\newcommand{\textop}[1]{\relax\ifmmode\mathop{\text{#1}}\else\text{#1}\fi}

	\geometry{margin=2cm}

	\title{%s}
	\author{%s}
	\date{%s}

	\begin{document}
		%^\maketitle
	\end{document}]],
	event.match:match '([^/]+)%.[^%.]+$',
	sys{'getent', 'passwd', env.USER}:split ':'[5],
	os.date '%d.%m.%Y')
end
