"setlocal noet
"setlocal list listchars=tab:»·,trail:·,extends:…,nbsp:‗
setlocal tabstop=2
setlocal shiftwidth=2
setlocal foldmethod=indent
setlocal noexpandtab

function! Add_New_Line(top, content, indent)
	if a:top == 1
		normal! gg
	endif
	let ex = "A\n"
	if a:indent == 1
		let ex = ex . "\t"
	endif
	if a:content == 'time'
		let ex = ex . strftime('%H:%M').' '
	else
		let ex = ex . a:content . ' '
	endif
    exec 'normal! ' . ex
	startinsert!
endfunction

nnoremap <buffer> tt :call Add_New_Line(0, '-', 1)<CR>
nnoremap <buffer> tn :call Add_New_Line(0, '-', 0)<CR>
nnoremap <buffer> tl :call Add_New_Line(1, 'time', 1)<CR>
"nnoremap <buffer> t ggO
