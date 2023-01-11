local sys = vim.fn.system
local env = vim.env
return function(loader, event) loader([[
	\documentclass{article}

	\usepackage[intlimits]{amsmath}
	\usepackage{geometry}
	\usepackage[shortlabels]{enumitem}
	\usepackage{graphicx}

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
