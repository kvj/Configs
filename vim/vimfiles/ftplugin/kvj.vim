"setlocal noet
"setlocal list listchars=tab:»·,trail:·,extends:…,nbsp:‗
setlocal tabstop=2
setlocal shiftwidth=2
setlocal foldmethod=indent
setlocal noexpandtab

let b:tp = ''
let b:tdef = ''
let b:date = 0
let b:cron = ''
let b:cr = ''
let b:amode = ''

let b:lastHourUpdate = 0

let s:signHour = 21000
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
	if a:content == 'time'
		let nowDate = strftime(s:dmFormat)
		let header = getline(1)
		if header != nowDate
			call append(0, nowDate)
			call cursor(1, 0)
		endif
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

"Loads decided tasks for that week from 04.ttt, loads recurrent tasks from
"05.ttt and puts them into every day of week
function! s:Mode07(lines, dt)
	"dt is basically start of week (Monday in our case)
	let lines04 = s:LoadFile('04.ttt')
	let parts = s:SearchForLines(lines04, '^W.*\s'.strftime(s:dmFormat, a:dt), 1)
	if len(parts) == 0
		"Not found
		echom 'Tasks were not found for this week'
		return
	endif
	let [idx, line, endidx] = parts[0]
	while idx<endidx
		let idx += 1
		let parts = matchlist(lines04[idx], '^\t-\s'.s:rxDate.'\?\('.s:rxTime.'\)\?\(.\+\)')
		"call s:PrintList(parts)
		if empty(parts) "Sub-child/text - skip
			continue
		endif
		let task = "\t- "
		let addidx = 1 "To top by default
		if !empty(parts[1]) "Have date
			let timeline = s:SearchForLines(a:lines, '^'.parts[2].'\/'.parts[3].'\s', 1)
			if len(timeline)>0 "Found
				"echo 'Search for: '.parts[6].':'.parts[7]
				let addidx = s:FindPlaceForChild(a:lines, timeline[0][0]+1, timeline[0][2], parts[6], parts[7], parts[8])
			else
				let task .= parts[1].' ' "Keep date anyway with task
			endif
		endif
		let task .= parts[5].parts[8]
		call insert(a:lines, task, addidx)
		let lastchild = s:SearchForLastChild(lines04, idx)
		call extend(a:lines, lines04[idx+1:lastchild], addidx+1)
	endwhile
	let lines05 = s:LoadFile('05.ttt') "Recurrent tasks
	let parts = s:SearchForLines(lines05, '^\t-\s', 1)
	let day = 0
	while day<7
		let date = s:AddToDate(a:dt, 'd', day)
		for part in parts
			let task = s:CheckCronStatement(part[1], date)
			if !empty(task)
				"echom 'Cron check: '.part[1].' OK '.task
				let timeline = s:SearchForLines(a:lines, '^'.strftime('%m\/%d', date).'\s', 1)
				if len(timeline)>0 "Found
					let m = matchlist(part[1], '^\t-\s'.s:rxTime.'\?')
					let addidx = s:FindPlaceForChild(a:lines, timeline[0][0]+1, timeline[0][2], m[2], m[3], task)
					call insert(a:lines, task, addidx)
					let lastchild = s:SearchForLastChild(lines05, part[0])
					"echom 'Add task: '.idx.' lastchild: '.lastchild.' from '.lines07[idx].' to '.lines07[lastchild]
					call extend(a:lines, lines05[part[0]+1:lastchild], addidx+1)
				else
					echom 'Can not find date in file: '.strftime('%m\/%d', date)
				endif
			endif
		endfor
		let day += 1
	endwhile
endfunction

" 1 = hour, 3 = min
let s:rexpHourMin = '^\t-\s\(\d\{2\}\)\?\(:\(\d\{2\}\)\)\?'

"Tries to find a place among children start..end according to hour (can be
"empty) and minutes
function! s:FindPlaceForChild(lines, start, end, hour, min, debug)
	"echo 'FindPlaceForChild: '.a:hour.':'.a:min.' to '.a:lines[a:start-1].' = '.a:debug. ' children = '.(a:end-a:start)
	let lastEmpty = -1
	for i in range(a:start, a:end)
		let m = matchlist(a:lines[i], s:rexpHourMin)
		"call s:PrintList(m)
		if empty(m)
			"No time found - it's OK. Invalid or child line
			continue
		endif
		"echo 'Line: '.a:lines[i]' '.m[1].' == '.a:hour.', '.m[3].' == '.a:min
		if empty(m[3])
			" No date/time in line
			let lastEmpty = i+1
			continue
		endif
		"call s:PrintList(m)
		if m[1] == a:hour
			"Hours are same
			if str2nr(m[3])>str2nr(a:min)
				" First time minute is after requested
				return i "Means 'add before this'
			endif
		else
			if str2nr(m[1])>str2nr(a:hour)
				" First time hour is after requested
				return i "Means 'add before this'
			endif
		endif
	endfor
	if lastEmpty == -1 || !empty(a:min)
		" Another option - we have a time but didn't find good place for it -
		" go to bottom
		let lastEmpty = a:end+1 "Very last
	endif
	" No match - add before lastEmpty
	return lastEmpty
endfunction

"Loads assigned tasks for that day from 07.ttt and puts those under selected
"hour
function! s:Mode01(lines, dt)
	let lines07 = s:LoadFile('07.ttt')
	let parts = s:SearchForLines(lines07, '^'.strftime(s:dmFormat, a:dt), 1)
	"echom 'Loaded 07: '.len(lines07).' parts: '.len(parts)
	if len(parts) == 0
		"Not found
		echom 'Tasks were not found for this day'
		return
	endif
	let [idx, line, endidx] = parts[0]
	"echom 'Tasks: from '.idx.' to '.endidx.', line: '.line
	while idx<endidx
		let idx += 1
		let parts = matchlist(lines07[idx], '^\t-\s'.s:rxTime.'\?\(.\+\)')
		if empty(parts) "Sub-child/text - skip
			continue
		endif
		let task = "\t- "
		let addidx = 1 "To big task by default
		if !empty(parts[1]) "Have time
			let timeline = s:SearchForLines(a:lines, '^'.parts[2].':00', 1)
			if len(timeline)>0 "Found
				let addidx = s:FindPlaceForChild(a:lines, timeline[0][0]+1, timeline[0][2], '', parts[3], parts[4])
				if parts[3] != '00' "Have minutes
					let task .= ':'.parts[3].' '
				endif
			endif
		endif
		let task .= parts[4]
		call insert(a:lines, task, addidx)
		let lastchild = s:SearchForLastChild(lines07, idx)
		"echom 'Add task: '.idx.' lastchild: '.lastchild.' from '.lines07[idx].' to '.lines07[lastchild]
		call extend(a:lines, lines07[idx+1:lastchild], addidx+1)
	endwhile
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

function! s:CursorHour()
	let dt = localtime()
	if dt-b:lastHourUpdate>s:cursorUpdateInterval*60
		exec 'sign unplace '.s:signHour.' file='.expand('%:p')
		let b:lastHourUpdate = dt
		let line = search('^'.strftime('%H', dt).':00', 'wn')
		if line>0
			exec 'sign place '.s:signHour.' line='.line.' name=tttHour file='.expand('%:p')
			"echom 'Cursor moved: '.strftime(s:hmFormat, dt)
		endif
	endif
endfunction

fun! CursorHourInterval(obj, subscription)
	echom 'Cursor moved at: '.strftime(s:hmFormat)
	call s:CursorHour()
endf

function! s:Enable_Markers()
	sign define tttHour text=>> texthl=Search
	if b:cr == 'hour'
		"Enable hour sign
		if exists('g:android')
			let dt = localtime()
			let dt = s:AddToDate(dt, 'i', 60-s:DateItem(dt, 'i'))
			call g:Android_Subscribe(g:Android_Execute('timer', {'interval': 60*60, 'time': dt}), function('CursorHourInterval'))
		else
			autocmd CursorHold,CursorHoldI,FocusGained,FocusLost <buffer> call s:CursorHour()
		endif
		call s:CursorHour()
	endif
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
			return 1
		endif
	endfor
	return 0
endfunction

function! JumpToWindow(path)
	let tab = tabpagenr()
	let tabs = tabpagenr('$')
	"echom 'Locating: '.a:path.' '.tab.' of '.tabs
	if s:ProcessTab(tab, a:path) == 1
		"Switched in currect tab
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
	return 0
endfunction

function! s:Enable_Hotkeys()
	if !exists('g:tttHotKeys')
		return
	endif
	if !exists('g:tttRoot')
		echom 'Root not defined. Please set g:tttRoot'
		return
	endif
	for [filePath, key] in items(g:tttHotKeys)
		let path = glob(g:tttRoot.'/'.filePath)
		exe 'nn <buffer> <silent><localleader>'.key.' :call JumpToWindow("'.substitute(path, '\\', '\\\\', 'g').'")<CR>'
		"echom 'Bound key '.key.' to file: '.path
	endfor
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

function! Set_Tag(remove, tag)
	let idx = line('.')
	let line = getline(idx)
	let m = matchlist(line, s:refillRexp)
	let str = m[1].m[2].m[3].m[6]
	if !a:remove
		let str .= m[7]
	endif
	if !empty(a:tag)
		let str .= ' #'.a:tag
	endif
	let str .= m[8]
	call setline('.', str)
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
	echo "Copied to register"
endfunction

function! b:Fold_Text()
	let nl = v:foldend - v:foldstart + 1
	let indent = s:Indent(getline(v:foldstart-1))
	let res = s:MakeIndent(indent+2)
	let res .= '++ '.nl.' --'
	return res
endfunction

setlocal foldtext=b:Fold_Text()
setlocal fillchars=fold:\ 

let maplocalleader = "t"

nnoremap <buffer> <silent><localleader>t :call Add_New_Line(0, '-', 1)<CR>
nnoremap <buffer> <silent><localleader>n :call Add_New_Line(0, '-', 0)<CR>
nnoremap <buffer> <silent><localleader>l :call Add_New_Line(1, 'time', 1)<CR>
nnoremap <buffer> <silent><localleader>f :call Fold_Marked()<CR>
nnoremap <buffer> <silent><localleader>y :call Insert_Template(1)<CR>
nnoremap <buffer> <silent><localleader>a :call Insert_Template(0)<CR>
nnoremap <buffer> <silent><localleader>c :call Make_Archive(1)<CR>
nnoremap <buffer> <silent><localleader>s :call Select_Tree()<CR>
nnoremap <buffer> <silent><localleader>r :call Copy_Tree(1)<CR>
nnoremap <buffer> <silent><localleader>o :call Set_Tag(1, 'ok')<CR>
nnoremap <buffer> <silent><localleader>i :call Set_Tag(1, 'list')<CR>
nnoremap <buffer> <silent><localleader>bv :call BeginSelectAll()<CR>
nnoremap <buffer> <silent><localleader>bb :call BeginCompile()<CR>
nnoremap <buffer> <silent><localleader>bn :call BeginOpen()<CR>

call Fold_Marked()
call s:Enable_Markers()
call s:Enable_Hotkeys()
