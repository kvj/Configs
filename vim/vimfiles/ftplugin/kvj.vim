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
let s:hmFormat = '%H:%M'
let s:rxTime = '\(\(\d\{2\}\):\(\d\{2\}\)\s\)'
let s:rxDate = '\(\(\d\{2\}\)\/\(\d\{2\}\)\s\)'

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
		"echom 'Found template: '.idx.' total: '.matches[1].', type: '.matches[2].' append: '.matches[3]
		let subs = '!!tmpl!!'
		if matches[1] == 'd'
			let _dt = a:dt
			if matches[3]
				"Have date modifier
				let _dt = s:AddToDate(a:dt, 'd', str2nr(matches[3]))
			endif
			let subs = strftime(matches[2], _dt)
		endif
		let a:lines[lineidx] = substitute(line, rexp, subs, "")
	endfor
endfunction

"Debug output for lists
function! s:PrintList(list)
	let idx = 0
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

"Looks for CRON element and checks whether provided date falls into it.
"Returns task text
function! s:CheckCronStatement(line, date)
	function! CheckCronItem(str, min, max, value)
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
	if !CheckCronItem(cron[0], 1, 31, s:DateItem(dt, 'd'))
		return ''
	endif
	if !CheckCronItem(cron[1], 1, 12, s:DateItem(dt, 'm'))
		return ''
	endif
	if !CheckCronItem(cron[2], 0, 6, s:DateItem(dt, 'wd'))
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
		let parts = matchlist(lines04[idx], '^\t-\s'.s:rxDate.'\?\(.\+\)')
		if empty(parts) "Sub-child/text - skip
			continue
		endif
		let task = "\t- "
		let addidx = 1 "To top by default
		if !empty(parts[1]) "Have date
			let timeline = s:SearchForLines(a:lines, '^'.parts[2].'\/'.parts[3].'\s', 1)
			if len(timeline)>0 "Found
				let addidx = timeline[0][2]+1 "After all children
			else
				let task .= parts[1].' ' "Keep date anyway with task
			endif
		endif
		let task .= parts[4]
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
					let addidx = timeline[0][2]+1 "After all children
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
				let addidx = timeline[0][2]+1 "After all children
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
		let b:lastHourUpdate = dt
		exec 'sign unplace '.s:signHour.' file='.expand('%:p')
		let line = search('^'.strftime('%H', dt).':00', 'wn')
		if line>0
			exec 'sign place '.s:signHour.' line='.line.' name=tttHour file='.expand('%:p')
			echom 'Cursor moved: '.strftime(s:hmFormat, dt)
		endif
	endif
endfunction

function! Enable_Markers()
	sign define tttHour text=>> texthl=Search
	if b:cr == 'hour'
		"Enable hour sign
		autocmd CursorHold,CursorHoldI,FocusGained,FocusLost <buffer> call s:CursorHour()
		echom 'Enabled hour cursor'
		call s:CursorHour()
	endif
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
nnoremap <buffer> <localleader>y :call Insert_Template(1)<CR>
nnoremap <buffer> <localleader>a :call Insert_Template(0)<CR>

call Fold_Marked()
call Enable_Markers()
