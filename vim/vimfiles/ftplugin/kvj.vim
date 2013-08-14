"setlocal noet
"setlocal list listchars=tab:»·,trail:·,extends:…,nbsp:‗
setlocal tabstop=2
setlocal shiftwidth=2
setlocal foldmethod=indent

function! Add_New_Line(top, content)
	if a:top == 1
		normal! gg
	endif
	let ex = "A\n\t"
	if a:content == 'time'
		let ex = ex . strftime('%H:%M').' '
	else
		let ex = ex . a:content . ' '
	endif
    exec 'normal! ' . ex
	startinsert!
endfunction

nnoremap <buffer> - :call Add_New_Line(0, '-')<CR>
nnoremap <buffer> l :call Add_New_Line(1, 'time')<CR>
"nnoremap <buffer> t ggO
