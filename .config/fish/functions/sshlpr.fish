function sshlpr -d 'Print over SSH';
	pdfunite $argv[3..] /dev/stdout \
		| ssh "$argv[1]" lpr -P "$argv[2]"
end

#function <...> -d 'Print somewhere'; sshlpr <host> <printer> $argv; end
