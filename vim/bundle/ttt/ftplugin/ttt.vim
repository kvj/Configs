"setlocal noet
"setlocal list listchars=tab:»·,trail:·,extends:…,nbsp:‗
setlocal tabstop=2
setlocal shiftwidth=2
setlocal foldmethod=indent
setlocal noexpandtab
setlocal foldignore=

let b:tp = ''
let b:tdef = ''
let b:date = 0
let b:cron = ''
let b:amode = ''
let b:qbar = ttt#ifDefined('b:qbar', 'def')
let b:jump = ''

let b:qbar_def = ['<Esc>tl<kOn> tl', '<Esc>tz tz', '<Esc>fe fe', '<Esc>fs<kOff> fs', '<Esc>tx tx', '<Esc>tt<kOn> tt', '<Esc>tu<kOn> tu', '<Esc>tn<kOn> tn']
" let b:qbar_report = ['h', 'l', 'a', 'w', 's', '<Space> sp', '<Enter> en', 'q', 'e']
"let b:qbar_01 = ['<Esc>fs<kOff> fs', '<Esc>tt<kOn> tt', '<Esc>tn<kOn> tn', 'to', '<Esc>tz tz', '<Esc>tw tw']
"let b:qbar_00 = ['<Esc>fs<kOff> fs', '<Esc>tl<kOn> tl', '<Esc>tx tx', '<Esc>tw tw', '<Esc>ggta<kOn> ta', '<Esc>tt<kOn> tt', '<Esc>tn<kOn> tn', 'to', 'ti']

let b:lastHourUpdate = 0

let s:signHour = 21000
let s:signTask = 22000
let s:cursorUpdateInterval = 5
let s:dmFormat = '%m/%d'
let s:dmyFileFormat = '%y%m%d'
let s:hmFormat = '%H:%M'
let s:rxTime = '\(\(\d\{2\}\):\(\d\{2\}\)\s\)'
let s:rxDate = '\(\(\d\{2\}\)\/\(\d\{2\}\)\s\)'

let s:handlers = ['gnome-open', 'kde-open', 'exo-open', 'xdg-open']

function! Add_New_Line(top, content, indent)
	if a:top == 1
		normal! gg
	endif
	if a:top == 2
		normal! G
	endif
	if a:content == 'time'
		let nowDate = strftime(s:dmFormat)
		let header = getline(1)
		if header != nowDate
			call append(0, nowDate)
			call cursor(1, 0)
		endif
	endif
	let ex = "o"
	if a:indent == 1
		let ex = ex . "\t"
	endif
	if a:indent == -1
		let ex = ex . "\<esc>0Di"
	endif
	if a:content == 'time'
		let ex = ex . strftime('%H:%M').' '
	else
		let ex = ex . a:content . ' '
	endif
    exec 'normal! ' . ex
	if exists('g:android')
		" Raise keyboard
		call g:Android_Execute('input', {'request': 'keyboard_show'})
	endif
	startinsert!
endfunction

"Extracts part of date and converts to number
function! s:DateItem(dt, item)
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
		return str2nr(strftime("%Y", a:dt))
	endif
	if a:item == 'h'
		return str2nr(strftime("%H", a:dt))
	endif
	if a:item == 'i'
		return str2nr(strftime("%M", a:dt))
	endif
	return 0
endfunction

"Adds value to date
function! s:AddToDate(dt, item, value)
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

"Prints date
function! s:ToDate(dt)
	return strftime("[%Y %b %d]", a:dt)
endfunction

"Prints date and time
function! s:ToDateTime(dt)
	return strftime("[%Y %b %d %T]", a:dt)
endfunction

function! s:SubstituteTemplates(lines, dt)
	let rexp = '{\([a-z]\)\(.\+\)[a-z]\([+\|-]\?\d\{1,2\}\)\?}'
	let lineidx = -1
	for line in a:lines
		let lineidx += 1
		let idx = match(line, rexp)
		if idx == -1
			continue
		endif
		let matches = matchlist(line, rexp)
		let subs = '!!tmpl!!'
		if matches[1] == 'd'
			let _dt = a:dt
			if matches[3]
				"Have date modifier
				let _dt = s:AddToDate(a:dt, 'd', str2nr(matches[3]))
			endif
			let subs = strftime(matches[2], _dt)
		endif
		"echo 'Found template: '.idx.' total: '.matches[1].', type: '.matches[2].' append: '.matches[3].' with '.subs
		let a:lines[lineidx] = substitute(line, rexp, subs, "")
	endfor
endfunction

"Debug output for lists
function! s:PrintList(list)
	let idx = 0
	echom 'PrintList: '.len(a:list)
	for item in a:list
		echom 'List['.idx.'] = '.item
		let idx += 1
	endfor
endfunction

"Loads file by relative path
function! s:LoadFile(name)
	let tmplfile = expand('%:h').'/'.a:name
	if !filereadable(tmplfile)
		echom 'File '.tmplfile.' is not readable'
		return []
	endif
	return readfile(tmplfile)
endfunction

"Returns indent of line
function! s:Indent(line)
	let tabs = matchstr(a:line, '^\t*')
	return len(tabs)
endfunction

"Returns index of last child
function! s:SearchForLastChild(lines, idx)
	let i = a:idx+1
	let lineindent = s:Indent(a:lines[a:idx])
	while i<len(a:lines)
		let line = a:lines[i]
		if s:Indent(line)<=lineindent && !empty(line)
			break
		endif
		let i += 1
	endwhile
	return i-1
endfunction

"Searches for all requested lines and returns those with indexes. Optionally
"returns children
function! s:SearchForLines(lines, rexp, children)
	let idx = 0
	let result = []
	while idx<len(a:lines)
		let line = a:lines[idx]
		if match(line, a:rexp) != -1
			"Found
			let item = [idx, line]
			call add(result, item)
			let lineindent = s:Indent(line)
			if a:children
				call add(item, s:SearchForLastChild(a:lines, idx))
			endif
		endif
		let idx += 1
	endwhile
	return result
endfunction

function! s:CheckCronItem(str, min, max, value)
	"echom 'CheckCronItem '.a:str.' '.a:min.'-'.a:max.' = '.a:value
	if '*' == a:str
		return 1
	endif
	if a:str[0] == '/'
		let inc = str2nr(a:str[1:])
		if !inc
			echom 'Invalid increment'
			return 0
		endif
		let idx = a:min
		"echom 'Inc: '.idx.' '.inc.' '.a:value
		while idx<=a:value && idx<=a:max
			if idx == a:value
				"Match
				return 1
			endif
			let idx += inc
		endwhile
		return 0 "No luck
	endif
	let values = split(a:str, ',')
	for val in values
		let nval = -1
		if val == 'SUN' 
			let nval = 0 
		elseif val == 'MON' 
			let nval = 1 
		elseif val == 'TUE' 
			let nval = 2 
		elseif val == 'WED' 
			let nval = 3 
		elseif val == 'THU' 
			let nval = 4 
		elseif val == 'FRI' 
			let nval = 5 
		elseif val == 'SAT' 
			let nval = 6
		else 
			let nval = str2nr(val) 
		endif
		"echom 'Iterate: '.val.'/'.nval.' = '.a:value
		if nval>a:value "Too big
			return 0
		endif
		if nval == a:value
			return 1 "Match
		endif
	endfor
	return 0
endfunction

"Looks for CRON element and checks whether provided date falls into it.
"Returns task text
function! s:CheckCronStatement(line, date)
	let rexp = '^\(.*\)\s\[\([A-Z0-9 ,\*\/]\+\)\]\(\s\/\)\?$'
	let parts = matchlist(a:line, rexp) "Parts: 1 - task, 2 - cron
	if empty(parts) || empty(parts[2])
		"No cron
		call s:PrintList(parts)
		echom 'No cron found: '.a:line
		return ''
	endif
	let cron = split(parts[2], ' ')
	"call s:PrintList(cron)
	if len(cron)<3
		echom 'Invalid cron: '.parts[2]
		return ''
	endif
	let task = parts[1].parts[3]
	let dt = a:date
	if len(cron)>3
		"Latest item is date of last exec
		"echom 'Parse: ['.cron[3].']'
		let dtParsed = matchlist(cron[3], '\(\d\{2\}\)\/\(\d\{2\}\)')
		if len(dtParsed)>0
			if s:DateItem(dt, 'm') == str2nr(dtParsed[1]) && s:DateItem(dt, 'd') == str2nr(dtParsed[2])
				"Exact
				return task. ' #next'
			else
				return ''
			endif
		endif
		echom 'Invalid date in cron'
		return ''
	endif
	if !s:CheckCronItem(cron[0], 1, 31, s:DateItem(dt, 'd'))
		return ''
	endif
	if !s:CheckCronItem(cron[1], 1, 12, s:DateItem(dt, 'm'))
		return ''
	endif
	if !s:CheckCronItem(cron[2], 0, 6, s:DateItem(dt, 'wd'))
		return ''
	endif
	return task
endfunction

function! Insert_Template(clear)
	let tmpl = input('Enter template: ', b:tdef)
	if empty(tmpl)
		return
	endif
	let dt = localtime()
	if b:date
		let days = str2nr(input('Enter day shift: '))
		let dt = s:AddToDate(dt, 'd', days)
	endif
	"echom 'Date: '.s:ToDate(dt).', day: '.s:DateItem(dt, 'd')
	let lines = s:LoadFile('.tmpl/'.b:tp.tmpl.'.ttt')
	"echom 'Template params: '.b:tp.' '.tmplfile.' lines: '.len(lines)
	if len(lines) == 0
		return
	endif
	if a:clear == 1
		normal! gg
	endif
	let @x = ''
	call s:SubstituteTemplates(lines, dt)
	if b:amode == '01'
		call s:Mode01(lines, dt)
	elseif b:amode == '07'
		call s:Mode07(lines, dt)
	endif
	for line in lines
		let @x .= line."\n"
	endfor
	if a:clear == 1
		normal! "xP
	else
		normal! "xp
	endif
	echom 'Inserted template '.tmpl
endfunction

" Returns all items in line
fun! s:GetAll(line, rexp)
	let idx = 0
	let result = []
	while 1
		let list = matchlist(a:line, a:rexp, idx)
		if empty(list)
			return result
		endif
		call add(result, list[1])
		let idx += match(a:line, a:rexp, idx)+len(list[0])
	endwhile
endf

function! s:Enable_Markers()
	sign define tttTask text=[] texthl=Todo
	call ttt#CursorTask()
	autocmd FileChangedShellPost,BufWritePost <buffer> call ttt#CursorTask()
endfunction

function! s:ProcessTab(tab, path)
	let tabnum = tabpagewinnr(a:tab, '$')
	let idx = 1
	let bufs = tabpagebuflist(a:tab)
	for bufno in bufs
		"echom 'Tab '.a:tab.', buf: '.bufno.', name: '.fnamemodify(bufname(bufno), ':p').' '.bufname(bufno)
		if fnamemodify(bufname(bufno), ':p') ==? a:path
			"Found buffer
			exe 'tabnext '.a:tab
			let winno = bufwinnr(bufname(bufno))
			"echom 'Will focus on: '.winno
			exe winno.'wincmd w'
			doau FileChangedShellPost
			return 1
		endif
	endfor
	return 0
endfunction

if !exists('*Jump_Window')
	function! Jump_Window(path)
		let tab = tabpagenr()
		let tabs = tabpagenr('$')
		" echom 'Locating: '.a:path.' '.tab.' of '.tabs
		if s:ProcessTab(tab, a:path) == 1
			"Switched in current tab
			return 1
		endif
		let idx = 1
		while idx <= tabs
			if s:ProcessTab(idx, a:path) == 1
				"Found in other tab
				return 1
			endif
			let idx += 1
		endwhile
		"Edit in current
		exe 'e '.a:path
		doau FileChangedShellPost
		return 0
	endfunction
endif

function! s:Enable_Hotkeys()
	if !exists('g:tttHotKeys')
		return
	endif
	for [filePath, key] in items(g:tttHotKeys)
		let path = ttt#findFile(filePath)
		exe 'nn <buffer> <silent><localleader>'.key.' :call Jump_Window("'.substitute(path, '\\', '\\\\', 'g').'")<CR>'
	endfor
endfunction

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

"Searches closest line with #begin from index
function! s:FindBegin(index)
	let idx = a:index
	while match(getline(idx), '\s*#begin ') == -1
		let idx -= 1
		if idx == -1
			return -1
		endif
	endwhile
	return idx
endfunction

"Selects block (without header)
function! BeginSelectAll()
	let beginIdx = s:FindBegin(line('.'))
	if beginIdx == -1
		echo 'Head of block not found'
		return
	endif
	let idx = beginIdx+1
	call cursor(idx, 0)
	let lastline = s:BufferSearchForLastChild(beginIdx)
	normal! V
	if lastline>idx
		exe 'normal! '.(lastline-idx).'j'
	endif
endfunction

"Parses header
function! s:BeginParseHeader(index)
	let m = matchlist(getline(a:index), '^\s*#begin\s\([a-z0-9]\+\)\s\(.\{-}\)\(\s/\)\?$')
	"call s:PrintList(m)
	return [m[1], s:ParseAttrs(m[2], 0)]
endfunction

function! s:OpenFile(location, ...)
	if has("win32")
		let command = '!start /min CMD /C START "" %s'
		silent execute printf(command, shellescape(a:location))
	else
		let s:uname = system("uname")
		if s:uname == "Darwin"
			"Max OSX
			let cmd = 'open ' . shellescape(a:location) . ' 2>&1'
			call system(cmd)
		else
			"Linux
			for handler in s:handlers + a:000
				if executable(handler)
					let cmd = shellescape(handler) . ' ' . shellescape(a:location) . ' 2>&1'
					call system(cmd)
					return
				endif
			endfor
		endif
	endif
endfunction

"Opens file from block
function! BeginOpen()
	let beginIdx = s:FindBegin(line('.'))
	if beginIdx == -1
		echo 'Head of block not found'
		return
	endif
	let [type, params] = s:BeginParseHeader(beginIdx)
	if !empty(get(params, 'file', ''))
		let fileName = fnamemodify(expand('%:p:h').'/'.params.file, ':p')
		call s:OpenFile(fileName)
	else
		echo 'File not found'
	endif
endfunction

"Compiles block starting from #begin
function! BeginCompile()
	let beginIdx = s:FindBegin(line('.'))
	if beginIdx == -1
		echo 'Head of block not found'
		return
	endif
	let [type, params] = s:BeginParseHeader(beginIdx)
	"echo 'Type: '.type.', file: '.params.file
	if get(g:kvjExtBlockConfig, type, {}) == {}
		"Not found
		echo 'Config ['.type.'] not found'
		return
	endif
	let lastline = s:BufferSearchForLastChild(beginIdx)
	let conf = g:kvjExtBlockConfig[type]
	let indent = s:Indent(getline(beginIdx+1))
	let inputFile = tempname()
	let cmd = conf.cmd
	let inputFile = ''
	if conf.input == 'lines'
		let inputFile = tempname()
		let idx = beginIdx+1
		let inp = []
		while idx<=lastline
			call add(inp, strpart(getline(idx), indent))
			let idx += 1
		endwhile
		call writefile(inp, inputFile)
		let cmd .= ' <'.fnameescape(inputFile)
	endif
	let fileName = ''
	if conf.output == 'file'
		let fileName = fnamemodify(expand('%:p:h').'/'.params.file, ':p')
		let cmd .= ' >'.fnameescape(fileName)
	endif
	"echo 'Exec: '.cmd.' Input: '.len(inp)
	exec 'silent !'.cmd
	if !empty(inputFile)
		call delete(inputFile)
	endif
	if !empty(fileName)
		call s:OpenFile(fileName)
	endif
endfunction

function! Select_Tree()
	let idx = line('.')
	let lastline = s:BufferSearchForLastChild(idx)
	normal! V
	if lastline>idx
		exe 'normal! '.(lastline-idx).'j'
	endif
endfunction

"Parses var=val;var=val to dictionary
function! s:ParseAttrs(text, index)
	let result = {}
	let pattern = '\([a-z]\+\)=\([a-zA-Z0-9_\\''\./\\,\{\}\[\]]\+\)\($\|;\)'
	let index = a:index
	let line = a:text
	let matched_list = matchlist(line, pattern, index)
	while len(matched_list)>0
		let result[matched_list[1]] = matched_list[2]
		let index = match(line, pattern, index)+len(matched_list[0])
		let matched_list = matchlist(line, pattern, index)
	endwhile
	return result
endfunction

function! s:Load()
	au! * <buffer>
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
			let vars = s:ParseAttrs(line, len('// ttt:'))
			for [var, value] in items(vars)
				exe 'let b:'.var.'='.value
			endfor
		endif
		let linenr += 1
	endwhile
	if foldsmade>0
		normal `x
	endif
	if exists('g:android')
		"Quickbar support
		if b:qbar != '' && exists('b:qbar_'.b:qbar)
			"echom 'Have qbar: '.b:qbar
			let cmd = "call g:Android_Execute('quickbar', {'items': b:qbar_".b:qbar."})"
			exe cmd
			autocmd BufLeave <buffer> call g:Android_Execute('quickbar', {'default': 1})
			exe "autocmd BufEnter <buffer> ".cmd
		endif
	endif
	if exists('g:tttNoNumber') && g:tttNoNumber == 1
		setlocal nonumber
	endif
	if b:jump == 'b'
		normal Gzz
	endif
endfunction

function! Make_Archive(addDate)
	let file = expand('%:h').'/.archive/'.expand('%:t')
	if a:addDate
		let file .= '.'.strftime(s:dmyFileFormat)
	endif
	exe '!cp '.expand('%:p').' '.file
	echom 'Copied to '.file
endfunction

let s:refillRexp = '^\(\t*\)\(.\s\)\?'.s:rxTime.'\?\(.\{-}\)\(\s#[a-z0-9]\+\)\?\( /\)\?$'
" 0 = full, 1 = indent, 2 = start sign, 3 = time, 6 = text, 7 = tag, 8 = fold sign

"Returns str of tabs num long
function! s:MakeIndent(num)
	let str = ''
	let i = 0
	while i<a:num
		let str .= "\t"
		let i += 1
	endwhile
	return str
endfunction

function! Copy_Tree(indent)
	let idx = line('.')
	let line = getline(idx)
	let m = matchlist(line, s:refillRexp)
	let indent = s:Indent(line)
	let endblock = s:BufferSearchForLastChild(idx)
	let block = s:MakeIndent(a:indent)
	if m[2]
		"Have start sign
		let block .= m[2]
	else
		"No sign
		let block .= "- "
	endif
	let block .= m[6].m[8]."\n"
	let i = idx+1
	while i<=endblock
		let l = getline(i)
		let id = s:Indent(l)
		if id>0
			"Have indent - means have text
			let newindent = id - indent + a:indent
			let mm = matchlist(l, '^\t*\(.*\)$')
			let block .= s:MakeIndent(newindent).mm[1]."\n"
		else
			"No indent = no text
			let block .= "\n"
		endif
		let i += 1
	endw
	let @" = block
	" echo "Copied to register"
endfunction

function! s:Fold_Text()
	let nl = v:foldend - v:foldstart + 1
	let indent = s:Indent(getline(v:foldstart-1))
	let res = s:MakeIndent(indent+2)
	let res .= '++ '.nl.' --'
	return res
endfunction

setlocal foldtext=s:Fold_Text()
setlocal fillchars=fold:\ 

let maplocalleader = "t"

nnoremap <buffer> <silent><localleader>u :call Add_New_Line(0, '-', 1)<CR>
nnoremap <buffer> <silent><localleader>n :call Add_New_Line(0, '-', 0)<CR>
nnoremap <buffer> <silent><localleader>t :call Add_New_Line(2, "\t-", -1)<CR>
nnoremap <buffer> <silent><localleader>l :call ttt#appendLog()<CR>
"nnoremap <buffer> <silent><localleader>y :call Insert_Template(1)<CR>
"nnoremap <buffer> <silent><localleader>a :call Insert_Template(0)<CR>
nnoremap <buffer> <silent><localleader>c :call Make_Archive(1)<CR>
nnoremap <buffer> <silent><localleader>s :call Select_Tree()<CR>
nnoremap <buffer> <silent><localleader>r :call Copy_Tree(1)<CR>
nnoremap <buffer> <silent><localleader>bv :call BeginSelectAll()<CR>
nnoremap <buffer> <silent><localleader>bb :call BeginCompile()<CR>
nnoremap <buffer> <silent><localleader>bn :call BeginOpen()<CR>
nnoremap <buffer> <silent><localleader>1 :call ttt#changeSign('-')<CR>
nnoremap <buffer> <silent><localleader>2 :call ttt#changeSign('=')<CR>
nnoremap <buffer> <silent><localleader>3 :call ttt#changeSign('#')<CR>
nnoremap <buffer> <silent><localleader>4 :call ttt#changeSign('~')<CR>
nnoremap <buffer> <silent><localleader>5 :call ttt#changeSign('?')<CR>
vnoremap <buffer> <silent> w <ESC>:call ttt#sendSelection('pebble', 'widget')<CR>

call s:Load()
call s:Enable_Markers()
call s:Enable_Hotkeys()
