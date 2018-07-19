let s:hmFormat = '%H:%M'

"Debug output for lists
fun! s:PrintList(list)
	let buf = 'List{'.len(a:list).'}['
	let idx = 0
	for item in a:list
		let buf .= ' ['.idx.']='.item
		let idx += 1
	endfor
	return buf.' ]'
endf

fun! s:Log(...)
	let buf = ''
	let idx = 0
	for item in a:000
		if idx>0
			let buf .= ', '
		endif
		let buf .= item
		let idx += 1
	endfor
	echom buf
endf

fun! s:AppendLineHere(append_mode, content, cursor)
	let ex = a:append_mode."\<esc>0Di"
	let ex = ex . a:content
    exec 'normal! ' . ex
	startinsert!
	call cursor(line('.'), a:cursor)
endf

"Extract part of date and converts to number
fun! s:DateItemPart(dt, item)
	if a:item == 'd'
		return str2nr(strftime("%d", a:dt))
	endif
	if a:item == 'wd'
		return str2nr(strftime("%w", a:dt))
	endif
	if a:item == 'w'
		return str2nr(strftime("%V", a:dt))
	endif
	if a:item == 'm'
		return str2nr(strftime("%m", a:dt))
	endif
	if a:item == 'y'
		return str2nr(strftime("%y", a:dt))
	endif
	if a:item == 'h'
		return str2nr(strftime("%H", a:dt))
	endif
	if a:item == 'i'
		return str2nr(strftime("%M", a:dt))
	endif
	return 0
endf

fun! s:RenderDate(arr)
	return Pad(a:arr[0]).'/'.Pad(a:arr[1]).'/'.Pad(a:arr[2])
endf

fun! s:RenderTime(arr)
	return Pad(a:arr[0]).':'.Pad(a:arr[1])
endf

fun! ttt#Close_Header()
	let idx = line('.')
	let indent = s:Indent(getline('.'))
	while idx>=1
		let line = getline(idx)
		let ind = s:Indent(line)
		if ind < indent
			" Parent
			let m = matchlist(line, '\(.\+:\)\( /\)\?$')
			if !empty(m)
				" Found
				call cursor(idx, 0)
				if m[2] " Already there
					return 0
				else
					call setline('.', m[1].' /')
					return 1
				endif
			endif
			if ind == 0
				return 0
			endif
		endif
		let idx -= 1
	endwhile
	return 0
endf

"Append log entry to file"
fun! ttt#Append_Log(time)
	let client = ''
	if exists('g:tttClient')
		let client = ' '.g:tttClient
	endif
	let tm = localtime()
	let dateArr = [s:DateItemPart(tm, 'y'), s:DateItemPart(tm, 'm'), s:DateItemPart(tm, 'd')]
	let timeArr = [s:DateItemPart(tm, 'h'), s:DateItemPart(tm, 'i'), 0]
	let date_line = s:BufferSearchForLines('^'.substitute(s:RenderDate(dateArr), '/', '\/', "").'.*:', 1)
	if len(date_line)
		" Already there - append after last child
		call cursor(date_line[0][2], 0)
		let content = "\t"
		if a:time
			let content = "\t".s:RenderTime(timeArr).client.":\n\t"
		endif
		return s:AppendLineHere('o', content, 4)
	endif
	let content = ''.s:RenderDate(dateArr).":\n\t"
	if a:time
		let content .= s:RenderTime(timeArr).client.":\n\t"
	endif
	let footer_line = []
	if b:footer != ''
		let footer_line = s:BufferSearchForLines('^'.b:footer.'$', 0)
	endif
	let append_mode = 'o'
	if len(footer_line) > 0
		call cursor(footer_line[len(footer_line)-1][0], 0)
		let append_mode = 'O'
	else
		normal! G
	endif
	return s:AppendLineHere(append_mode, content, 4)
endf

"Returns indent of line
function! s:Indent(line) " 1
	let tabs = matchstr(a:line, '^\t*')
	return len(tabs)
endfunction

fun! s:FillChars(num, char)
	let res = ''
	let idx = 0
	while idx < a:num
		let idx += 1
		let res .= a:char
	endwhile
	return res
endf

fun! s:ChangeSignHere(sign)
	let line = getline('.')
	let parsed = ParseLine(line)
	if parsed['type'] == ''
		return 0
	endif
	let ind = s:Indent(line)
	let text = s:FillChars(ind, "\t") . a:sign . strpart(line, ind + 1)
	call setline('.', text)
	return 1
endf

fun! ttt#changeSign(sign)
	return s:ChangeSignHere(a:sign)
endf

let s:signTask = 22000

function! s:BufferSearchForLines(rexp, children, ...)
	let idx = 1
	let end = line('$')
	if len(a:000) == 2
		let idx = a:1
		let end = a:2
	endif
	let result = []
	while idx<=end
		let line = getline(idx)
		if match(line, a:rexp) != -1
			"Found
			let item = [idx, line]
			call add(result, item)
			let lineindent = s:Indent(line)
			if a:children
				call add(item, s:BufferSearchForLastChild(idx))
			endif
		endif
		let idx += 1
	endwhile
	return result
endfunction

function! s:BufferSearchForLastChild(idx)
	let i = a:idx+1
	let lineindent = s:Indent(getline(a:idx))
	while i<=line('$')
		let line = getline(i)
		if s:Indent(line)<=lineindent && !empty(line)
			break
		endif
		let i += 1
	endwhile
	return i-1
endfunction

fun! ttt#CursorTask()
	if !exists('b:taskSigns')
		let b:taskSigns = 0
	endif
	let signNo = 0
	let tasklines = s:BufferSearchForLines('\t\+=!\{,5}\s.*$', 0)
	for task in tasklines
		let signType = 'tttTask'
		" Put mark
		exec 'sign unplace '.(s:signTask+signNo).' buffer='.bufnr('%')
		exec 'sign place '.(s:signTask+signNo).' line='.task[0].' name='.signType.' buffer='.bufnr('%')
		let signNo += 1
	endfor
	if signNo<b:taskSigns
		for i in range(signNo, b:taskSigns-1)
			" echom 'Remove sign' i
			exec 'sign unplace '.(s:signTask+i).' buffer='.bufnr('%')
		endfor
	endif
	let b:taskSigns = signNo
endf

fun! ttt#Add_New_Line(top, content, indent)
	if a:top == 1
		normal! gg
	endif
	if a:top == 2
		normal! G
	endif
	let ex = "o"
	if a:indent == 1
		let ex = ex . "\t"
	endif
	if a:indent == -1
		let ex = ex . "\<esc>0Di"
	endif
	if a:content == 'time'
		let ex = ex . strftime(s:hmFormat).' '
	else
		let ex = ex . a:content . ' '
	endif
    exec 'normal! ' . ex
	if exists('g:android')
		" Raise keyboard
		call g:Android_Execute('input', {'request': 'keyboard_show'})
	endif
	startinsert!
endf

