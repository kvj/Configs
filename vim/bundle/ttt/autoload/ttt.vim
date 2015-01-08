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
	call Log('Send selection', start, finish, len(lines))
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

"Parse date
"m[2] - first date, m[4] - first time, m[7] - second date, m[9] - second time
let s:rxParseDate = '^[\[{]\('.s:rxDateSymbols.'\)\(\s'.s:rxTimeSymbols.'\)\?\(\s\?-\s\?\('.s:rxDateSymbols.'\)\?\(\s'.s:rxTimeSymbols.'\)\?\)\?[\]}]$'
fun! ParseTimestamp(text)
	let m = matchlist(a:text, s:rxParseDate)
	if len(m) == 0
		return {}
	endif
	let dtStart = ParseDate(m[2])
	let tmStart = ParseTime(m[4])
	let dtFinish = ParseDate(m[7])
	let tmFinish = ParseTime(m[9])
	if len(tmFinish)>0 && len(dtFinish) == 0
		let dtFinish = copy(dtStart)
	endif
	let result = {}
	let result['start'] = a:text[0]
	let result['finish'] = a:text[-1:]
	if len(dtStart) > 0
		"Have start date
		let result['dtStart'] = dtStart
		if len(dtFinish) > 0
			"Also have finish date
			let result['dtFinish'] = dtFinish
			if len(tmFinish) > 0
				"Time set only when both are set
				let result['tmStart'] = tmStart
				let result['tmFinish'] = tmFinish
			endif
		else
			"Only start time
			if len(tmStart) > 0
				let result['tmStart'] = tmStart
			endif
		endif
	endif
	return result
endf

"Parse line into prefix, text, date-time and tags. Return dictionary
let s:parseLineRexp = '^\t*\(\([^\s]\)\(\!*\)\s\+\)\?\(.*\)\s*$'
fun! ParseLine(line)
	let m = matchlist(a:line, s:parseLineRexp)
	let result = {
		\'type': m[2],
		\'priority': len(m[3]),
		\'tags': []
	\}
	let text = m[4]
	"call Log('ParseLine', result['type'], text, a:line)
	while 1
		let result['text'] = text
		let wordstart = strridx(text, ' ')
		if -1 == wordstart
			return result
		endif
		let word = strpart(text, wordstart+1)
		if word[0] == '#'
			let result['tags'] = add(result['tags'], strpart(word, 1))
			"call Log('Found tag', text, word)
		elseif word == '/'
			let result['collapsed'] = 1
		elseif (word[-1:] == ']') || (word[-1:] == '}') 
			let datestart = max([strridx(text, '['), strridx(text, '{')])
			if -1 == datestart
				return result
			endif
			let datetext = strpart(text, datestart)
			let parsedDate = ParseTimestamp(datetext)
			if has_key(parsedDate, 'dtStart')
				"call Log('Have date', parsedDate['dtStart'][0])
				let result['date'] = parsedDate
			endif
			let wordstart = datestart - 1
		else
			return result
		endif
		let text = strpart(text, 0, wordstart)
	endwhile
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

fun! CompareDateTime(arr1, arr2)
	let i = 0
	while i < 3
		if a:arr1[i] > a:arr2[i]
			return 1
		endif
		if a:arr1[i] < a:arr2[i]
			return -1
		endif
		let i += 1
	endwhile
	return 0
endf

fun! ProcessLines(lines, from, to, accept, report, now, tags)
	"call Log('ProcessLines', a:from, a:to, a:accept)
	let tm = a:now
	let dateArr = [DateItemPart(tm, 'y'), DateItemPart(tm, 'm'), DateItemPart(tm, 'd')]
	let result = []
	let i = a:from
	while i < a:to
		let line = a:lines[i]
		let p = ParseLine(line)
		let hasTag = 0
		let acceptLine = a:accept
		for t in a:tags
			if index(p['tags'], strpart(t, 1)) != -1
				let hasTag = 1
				if t[0] == '+'
					let acceptLine = 1 "Tag with plus - accept this and children
				else
					let acceptLine = 0 "Tag with minus - ignore this and children
				endif
				break
			endif
		endfor
		let ignoreTask = 0
		if !acceptLine "Line not accepted
			let ignoreTask = 1
		endif
		if p['type'] == '' "No type for this line
			let ignoreTask = 1
		endif
		if !ignoreTask && has_key(a:report, 'type') && (stridx(a:report['type'], p['type']) == -1) "Type requested but does not fit
			let ignoreTask = 1
		endif
		if !ignoreTask && has_key(p, 'date')
			" Task with date
			let dt = p['date']
			let startCompare = CompareDateTime(dt['dtStart'], dateArr)
			let finishCompare = startCompare
			if has_key(dt, 'dtFinish')
				let finishCompare = CompareDateTime(dt['dtFinish'], dateArr)
			endif
			let pos = 0 " Position of task related to now
			if startCompare < 0 && finishCompare < 0
				let pos = 1
			endif
			if startCompare > 0 && finishCompare > 0
				let pos = -1
			endif
			if pos == -1 && dt['start'] == '['
				" Before now and closed start bracket - not due
				let ignoreTask = 1
			endif
			if pos == 1 && dt['finish'] == ']'
				" After now and closed end bracket - not from
				let ignoreTask = 1
			endif
			if get(a:report, 'all', 0) "Useful for inboxes - ignore dates
				let ignoreTask = 0
			endif
		endif
		if !ignoreTask && get(a:report, 'calendar', 0) && !has_key(p, 'date')
			let ignoreTask = 1 "Stop processing
		endif
		let end = EndOfIndent(a:lines, i, a:to)
		if !ignoreTask "No other problems - add
			let task = {
				\'index': i,
				\'text': p['text'],
				\'priority': p['priority'],
				\'type': p['type']
			\}
			if has_key(p, 'date')
				let task['date'] = p['date']
			endif
			if end > i
				let task['children'] = end - i
			endif
			call add(result, task)
		endif
		if end > i
			if !ignoreTask && get(a:report, 'root', 0)
				" Only first level tasks
			else
				call extend(result, ProcessLines(a:lines, i+1, end+1, acceptLine, a:report, a:now, a:tags))
			endif
		endif
		let i = end + 1
	endwhile
	return result
endf

"Parse one file
fun! ParseOneFile(path, report, now, tags)
	let lines = ReadOneFile(a:path)
	return ProcessLines(lines, 0, len(lines), 1, a:report, a:now, a:tags)
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

"Parse report etc
fun! RenderReport(name, now)
	func! SortTasks(i1, i2)
		let dates = has_key(a:i1, 'date') + has_key(a:i2, 'date')
		let pdiff = a:i1['priority'] - a:i2['priority']
		if dates == 2
			if pdiff != 0
				return -pdiff
			endif
			let starts = CompareDateTime(a:i1['date']['dtStart'], a:i2['date']['dtStart'])
			if starts != 0
				return starts
			endif
			if has_key(a:i1['date'], 'tmStart') && has_key(a:i2['date'], 'tmStart')
				return CompareDateTime(a:i1['date']['tmStart'], a:i2['date']['tmStart'])
			endif
			return has_key(a:i1['date'], 'tmStart') - has_key(a:i2['date'], 'tmStart')
		endif
		if pdiff != 0
			return -pdiff
		endif
		if dates != 0
			return dates
		endif
		return -pdiff
	endf
	let report = g:tttReports[a:name]
	let lines = []
	let dateArr = [DateItemPart(a:now, 'y'), DateItemPart(a:now, 'm'), DateItemPart(a:now, 'd')]
	call add(lines, 'For '.RenderDate(dateArr))
	let data = [{'title': 'Root'}]
	for item in report['parts']
		let lines = add(lines, item['title'])
		let data = add(data, {'title': item['title']})
		let files = FindFiles(item['files'])
		let tags = []
		if has_key(item, 'tags')
			let tags = split(item['tags'], ' ')
		endif
		"call Log('Part:', item['files'], len(files))
		let alltasks = []
		for f in files
			let tasks = ParseOneFile(f, item, a:now, tags)
			for t in tasks
				let t['file'] = f
			endfor
			call extend(alltasks, tasks)
			"call Log('File', f, len(tasks))
		endfor
		call sort(alltasks, "SortTasks")
		"call Log('Total tasks:', len(alltasks))
		for t in alltasks
			"call Log('Task:', t['text'])
			let txt = "\t".t['type'].FillChars(t['priority'], '!').' '
			if has_key(t, 'date')
				let dt = t['date']
				let startCompare = CompareDateTime(dt['dtStart'], dateArr)
				let finishCompare = startCompare
				if startCompare < 0
					let txt .= RenderDate(dt['dtStart']).' ~ '
				endif
				if startCompare > 0
					let txt .= '~ ' . RenderDate(dt['dtStart']).' '
				endif
				if has_key(dt, 'dtFinish') && CompareDateTime(dt['dtStart'], dt['dtFinish']) != 0
					let finishCompare = CompareDateTime(dt['dtFinish'], dateArr)
					if finishCompare != 0
						let txt .= '~ ' . RenderDate(dt['dtFinish']).' '
					endif
				endif
				if has_key(dt, 'tmStart')
					let txt .= RenderTime(dt['tmStart']).' '
					if has_key(dt, 'tmFinish')
						let txt .= '- '.RenderTime(dt['tmFinish']).' '
					endif
				endif
			endif
			let txt .= t['text']
			if has_key(t, 'children')
				let txt .= ' ['.t['children'].']'
			endif
			let lines = add(lines, txt)
			let data = add(data, {'task': t})
		endfor
	endfor
	let b:data = data
	return lines
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

fun! Jump2Task(mode)
	let line = line('.') - 1
	if line >= len(b:data)
		return 0
	endif
	let item = b:data[line]
	"call Log('Jump2Task', line, len(b:data), has_key(item, 'task'))
	if has_key(item, 'task')
		let task = item['task']
		if FindBuffer(task['file'], 'f')
			"call Log('Jumped', task['file'])
		else
			if a:mode == 1
				call MakeJump2Split()
			endif
			exe 'e '.task['file']
		endif
		exe ''.(task['index']+1)
	endif
	return 1
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

"Change b:currentTime and refresh report
fun! MoveDate(dir)
	if a:dir == ''
		"Reset to now
		let b:currentTime = localtime()
	elseif a:dir == '+'
		let b:currentTime = AddToDate(b:currentTime, 'd', 1)
	elseif a:dir == '-'
		let b:currentTime = AddToDate(b:currentTime, 'd', -1)
	endif
	call RefreshReport()
endf

fun! RefreshReport()
	let line = line('.')
	let lines = RenderReport(b:reportName, b:currentTime)
	setlocal modifiable
	"call Log('showReport', len(lines))
	%d
	call setline(1, lines)
	setlocal nomodifiable
	call cursor(line, 1)
	call ttt#CursorTask()
endf

fun! SaveReload()
	wa
	call RefreshReport()
endf

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

fun! ttt#showReport(name, autoCreate)
	let currentWin = winnr()
	let bufferName = '['.s:reportPrefix.''.a:name.']'
	let reportName = a:name
	if a:name == ''
		let reportName = g:tttReportDefault
	endif
	if FindBuffer(bufferName, 't') == 0
		if !a:autoCreate
			return 0
		endif
		if a:autoCreate == 2
			call MakeJump2Split()
		endif
        execute 'silent keepjumps hide edit'.bufferName
		setlocal buftype=nofile
		setlocal noswapfile
		let b:qbar = 'report'
		let b:expanded = 0
		setlocal ft=ttt
		nnoremap <script> <buffer> <silent> <CR> :call Jump2Task(0)<CR>
		nnoremap <script> <buffer> <silent> <Space> :call Jump2Task(1)<CR>
		nnoremap <script> <buffer> <silent> r :call RefreshReport()<CR>
		nnoremap <script> <buffer> <silent> q :call MoveDate('-')<CR>
		nnoremap <script> <buffer> <silent> e :call MoveDate('+')<CR>
		nnoremap <script> <buffer> <silent> w :call MoveDate('')<CR>
		nnoremap <script> <buffer> <silent> s :call SaveReload()<CR>
		nnoremap <script> <buffer> <silent> a :call AppendLine(ttt#ifDefined('g:tttInbox', ''), "\t- ")<CR>
		nnoremap <script> <buffer> <silent> l :call AppendLog(ttt#ifDefined('g:tttLog', ''), "\t- ")<CR>
		nnoremap <script> <buffer> <silent> x :call SelectBlock()<CR>
		nnoremap <script> <buffer> <silent> y :call CopyBlock()<CR>
		nnoremap <script> <buffer> <silent> k :call CollapseExpand()<CR>
		nnoremap <script> <buffer> <silent> 1 :call ChangeSign('-')<CR>
		nnoremap <script> <buffer> <silent> 2 :call ChangeSign('=')<CR>
		nnoremap <script> <buffer> <silent> 3 :call ChangeSign('#')<CR>
		nnoremap <script> <buffer> <silent> 4 :call ChangeSign('~')<CR>
		nnoremap <script> <buffer> <silent> 5 :call ChangeSign('?')<CR>
		let b:currentTime = localtime()
	else
		"call Log('jumped to Report', bufferName)
	endif
	let b:reportName = reportName
	call RefreshReport()
	" exe ''.currentWin.'wincmd w'
	return 1
endf

if exists('g:tttShowReport')
	au VimEnter * call ttt#showReport(g:tttShowReport, 2)
endif
