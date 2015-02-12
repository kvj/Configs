let s:signTask = 22000
let s:dmFormat = '%m/%d'
let s:dmyFileFormat = '%y%m%d'
let s:hmFormat = '%H:%M'
let s:rxDateSymbols = '\([0-9\/]\+\)'
let s:rxTimeSymbols = '\([0-9:]\+\)'
let s:rxTime = '\(\d\{1,2\}\):\(\d\{2\}\)' "H:mm
let s:rxDate = '\(\(\d\{2\}\)/\)\?\(\d\{1,2\}\)/\(\d\{1,2\}\)' "yy/m/d

let s:reportPrefix = 'Report'

fun! Log(...)
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

fun! ttt#sendSelection(dest, name)
	let start = line("'<")
	let finish = line("'>")
	if start>finish
		let start = finish
		let finish = line("'<")
	endif
	let lines = []
	let ind = Indent(getline(start))
	let idx = start
	while idx <= finish
		let line = strpart(getline(idx), ind)
		let line = substitute(line, '\t', '  ', 'g')
		call add(lines, line)
		let idx += 1
	endwhile
	if !exists('g:android')
		return 0
	endif
	call g:Android_Execute('widget', 
				\ {'lines': lines,
				\  'dest':  a:dest,
				\  'name':  a:name
				\ })
	return 1
endf

function! BufferSearchForLines(rexp)
	let idx = 1
	let end = line('$')
	let result = []
	while idx<=end
		let line = getline(idx)
		if match(line, a:rexp) != -1
			"Found
			let item = [idx, line]
			call add(result, item)
		endif
		let idx += 1
	endwhile
	return result
endfunction

fun! ttt#CursorTask()
	if !exists('b:taskSigns')
		let b:taskSigns = 0
	endif
	let signNo = 0
	let tasklines = BufferSearchForLines('\t\+=!\{,5}\s.*$')
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

fun! FindBuffer(name, mode)
	"Searches for a named buffer
	let windows = winnr('$')
	let idx = 1
	while idx <= windows
		let bufno = winbufnr(idx)
		let bname = bufname(bufno)
		let found = 0
		if (a:mode == 'f') && (fnamemodify(bname, ':p') ==? a:name)
			let found = 1
		endif
		if  (a:mode == 't') && (bname ==? a:name)
			let found = 1
		endif
		if found
			"Found buffer
			"echom 'Will focus on: '.idx.' - '.bufname(bufno)
			exe ''.idx.'wincmd w'
			doau FileChangedShellPost
			return 1
		endif
		let idx += 1
	endwhile
	return 0
endfunction

"Expand file list definition into array of paths
fun! FindFiles(file)
	let fname = a:file
	if 0 == stridx(fname, 'ttt:') && exists('g:tttRoot')
		"ttt files
		let fname = g:tttRoot.'/'.strpart(fname, 4)
	endif
	return split(glob(fname), '\n')
endf

"Read file into list of lines
fun! ReadOneFile(path)
	return readfile(a:path)
endf

"Return indent of line
fun! Indent(line)
	let tabs = matchstr(a:line, '^\t*')
	return len(tabs)
endf

fun! FillChars(num, char)
	let res = ''
	let idx = 0
	while idx < a:num
		let idx += 1
		let res .= a:char
	endwhile
	return res
endf

"Extract part of date and converts to number
fun! DateItemPart(dt, item)
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

"Parse date and returns array of 3 [y, m, d]
fun! ParseDate(text)
	let m = matchlist(a:text, s:rxDate)
	if len(m)>0
		let y = 0
		if len(m[1])>0
			let y = str2nr(m[2])
		else
			let y = DateItemPart(localtime(), 'y')
		endif
		return [y, str2nr(m[3]), str2nr(m[4])]
	endif
	return []
endf

"Parse time and returns array of 3 [h, m, 0]
fun! ParseTime(text)
	let m = matchlist(a:text, s:rxTime)
	if len(m)>0
		return [str2nr(m[1]), str2nr(m[2]), 0]
	endif
	return []
endf

fun! EndOfIndent(lines, from, to)
	let indent = Indent(a:lines[a:from])
	let i = a:from + 1
	while i < a:to
		let ind = Indent(a:lines[i])
		if (ind <= indent) && (ind != -1)
			return i - 1
		endif
		let i += 1
	endwhile
	return a:to - 1
endf

fun! EndOfIndentBuffer(from, to)
	let indent = Indent(getline(a:from))
	let i = a:from + 1
	while i <= a:to
		let ind = Indent(getline(i))
		if (ind <= indent) && (ind != -1)
			return i - 1
		endif
		let i += 1
	endwhile
	return a:to
endf

fun! Pad(num)
	if a:num<10
		return '0'.a:num
	endif
	return ''.a:num
endf

fun! RenderDate(arr)
	return Pad(a:arr[0]).'/'.Pad(a:arr[1]).'/'.Pad(a:arr[2])
endf

fun! RenderTime(arr)
	return Pad(a:arr[0]).':'.Pad(a:arr[1])
endf

fun! MakeJump2Split()
	let curr = winnr()
	let foundWin = -1
	let size = -1
	let idx = 1
	while idx <= winnr('$')
		if idx != curr
			let s = winwidth(idx)*winheight(idx)
			if s > size
				let foundWin = idx
				let size = s
			endif
		endif
		let idx += 1
	endwhile
	if foundWin != -1 "Found big enough win
		exe ''.foundWin.'wincmd w'
		return foundWin
	endif
	" Have to split
	let cmd = ''.ttt#ifDefined('g:tttSplitSize', '').'sp'
	"if winwidth(0) > winheight(0)
	"	let cmd = 'vs'
	"endif
	exe cmd
	return winnr()
endf

fun! ChangeSignHere(sign)
	let line = getline('.')
	let parsed = ParseLine(line)
	if parsed['type'] == ''
		return 0
	endif
	let ind = Indent(line)
	let text = FillChars(ind, "\t") . a:sign . strpart(line, ind + 1)
	call setline('.', text)
	return 1
endf

fun! SelectHere()
	let idx = line('.')
	let lastline = EndOfIndentBuffer(idx, line('$'))
	normal! V
	if lastline>idx
		exe 'normal! '.(lastline-idx).'j'
	endif
endf

fun! SelectBlock()
	call Jump2Task(1)
	call SelectHere()
endf

fun! CopyBlock()
	let curr = winnr()
	call Jump2Task(1)
	call SelectHere()
	normal! y
	exe ''.curr.'wincmd w'
endf

fun! ChangeSign(sign)
	let curr = winnr()
	call Jump2Task(1)
	call ChangeSignHere(a:sign)
	exe ''.curr.'wincmd w'
endf

fun! ttt#changeSign(sign)
	return ChangeSignHere(a:sign)
endf

fun! ttt#ifDefined(name, def)
	if exists(a:name)
		exe 'let val = '.a:name
		return val
	endif
	return a:def
endf

fun! AppendLineHere(content, cursor)
	edit
	normal! G
	let ex = "o\<esc>0Di"
	let ex = ex . a:content
    exec 'normal! ' . ex
	if exists('g:android')
		" Raise keyboard
		call g:Android_Execute('input', {'request': 'keyboard_show'})
	endif
	startinsert!
	call cursor(line('.'), a:cursor)
endf

fun! AppendLine(file, content)
	if a:file == ''
		call Log('Target not defined')
		return 0
	endif
	let paths = FindFiles(a:file)
	if FindBuffer(paths[0], 'f')
		"call Log('Jumped', task['file'])
	else
		call MakeJump2Split()
		exe 'e '.paths[0]
	endif
	return AppendLineHere(a:content, 99)
endf

"Append log entry to file"
fun! ttt#appendLog()
	let client = ''
	if exists('g:tttClient')
		let client = ' '.g:tttClient
	endif
	let tm = localtime()
	let dateArr = [DateItemPart(tm, 'y'), DateItemPart(tm, 'm'), DateItemPart(tm, 'd')]
	let timeArr = [DateItemPart(tm, 'h'), DateItemPart(tm, 'i'), 0]
	let content = ''.RenderDate(dateArr).' '.RenderTime(timeArr).client.":\n\t"
	return AppendLineHere(content, 2)
endf

fun! AppendLog(file, content)
	if a:file == ''
		call Log('Target not defined')
		return 0
	endif
	let paths = FindFiles(a:file)
	if !FindBuffer(paths[0], 'f')
		call MakeJump2Split()
		exe 'e '.paths[0]
	endif
	let content = a:content
	let tm = localtime()
	let dateArr = [DateItemPart(tm, 'y'), DateItemPart(tm, 'm'), DateItemPart(tm, 'd')]
	let timeArr = [DateItemPart(tm, 'h'), DateItemPart(tm, 'i'), 0]
	let content .= ' ['.RenderDate(dateArr).' '.RenderTime(timeArr).']'
	return AppendLineHere(content, 4)
endf

"Add value to date
function! AddToDate(dt, item, value)
	if !a:value
		return a:dt
	endif
	if a:item == 'd'
		return a:dt+a:value*60*60*24
	endif
	if a:item == 'm'
		"TODO: Implement
		return a:dt
	endif
	if a:item == 'y'
		"TODO: Implement
		return a:dt
	endif
	if a:item == 'h'
		return a:dt+a:value*60*24
	endif
	if a:item == 'i'
		return a:dt+a:value*60
	endif
	return a:dt
endfunction

fun! CollapseExpand()
	let size = 10
	if exists('g:tttExpandSize')
		let size = g:tttExpandSize
	endif
	if b:expanded
		exe 'resize '.b:collapseSize
		let b:expanded = 0
	else
		let b:expanded = 1
		let b:collapseSize = winheight('.')
		exe 'resize '.(b:collapseSize+size)
	endif
endf
