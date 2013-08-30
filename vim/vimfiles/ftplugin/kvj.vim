"setlocal noet
"setlocal list listchars=tab:»·,trail:·,extends:…,nbsp:‗
setlocal tabstop=2
setlocal shiftwidth=2
setlocal foldmethod=indent
setlocal noexpandtab

let b:tp = ''
let b:tdef = ''
let b:cron = ''
let b:cursor = 0

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

function! Insert_Template()
	let tmpl = input('Enter template: ', b:tdef)
	if empty(tmpl)
		return
	endif
	let tmplfile = expand('%:h').'/.tmpl/'.b:tp.tmpl.'.ttt'
	let tmplfile = expand(tmplfile)
	let lines = readfile(tmplfile)
	echom 'Template params: '.b:tp.' '.tmplfile.' lines: '.len(lines)
	if len(lines) == 0
		return
	endif
	normal! gg
	for line in lines
	endfor
endfunction

function! Fold_Marked()
	normal mx
	let linenr = 0
	let foldsmade = 0
	while linenr <= line("$")
		let line = getline(linenr)
		let matched = matchstr(line, '^.*\s/$')
		if !empty(matched)
			call cursor(linenr+1, 0)
			normal zc
			let foldsmade += 1
		endif
		"Set buffer parameters
		let matched = matchstr(line, '^// ttt:\(.\+\)$')
		if !empty(matched)
			let index = len('// ttt:')
			let pattern = '\([a-z]\+\)=\([a-zA-Z0-9_\\'']\+\)\($\|;\)'
			let matched_list = matchlist(line, pattern, index)
			while len(matched_list)>0
				"echom 'Found settings: '.len(matched_list).' '.len(matched_list[0])
				exe 'let b:'.matched_list[1].'='.matched_list[2]
				let index = match(line, pattern, index)+len(matched_list[0])
				let matched_list = matchlist(line, pattern, index)
			endwhile
		endif
		let linenr += 1
	endwhile
	if foldsmade>0
		normal `x
	endif
endfunction

let maplocalleader = "t"

nnoremap <buffer> <localleader>t :call Add_New_Line(0, '-', 1)<CR>
nnoremap <buffer> <localleader>n :call Add_New_Line(0, '-', 0)<CR>
nnoremap <buffer> <localleader>l :call Add_New_Line(1, 'time', 1)<CR>
nnoremap <buffer> <localleader>f :call Fold_Marked()<CR>
nnoremap <buffer> <localleader>y :call Insert_Template()<CR>

call Fold_Marked()
