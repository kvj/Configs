"setlocal noet
"setlocal list listchars=tab:»·,trail:·,extends:…,nbsp:‗
setlocal tabstop=2
setlocal shiftwidth=2
setlocal foldmethod=syntax
setlocal noexpandtab
setlocal foldignore=

fun! ttt#ifDefined(name, def)
	if exists(a:name)
		exe 'let val = '.a:name
		return val
	endif
	return a:def
endf

let b:tp = ''
let b:tdef = ''
let b:date = 0
let b:cron = ''
let b:amode = ''
let b:qbar = ttt#ifDefined('b:qbar', 'def')
let b:jump = ''
let b:footer = '---'

let b:qbar_def = ['<Esc>tl<kOn> tl', '<Esc>tz tz', '<Esc>fe fe', '<Esc>fs<kOff> fs', '<Esc>tx tx', '<Esc>tt<kOn> tt', '<Esc>tu<kOn> tu', '<Esc>tn<kOn> tn']
" let b:qbar_report = ['h', 'l', 'a', 'w', 's', '<Space> sp', '<Enter> en', 'q', 'e']
"let b:qbar_01 = ['<Esc>fs<kOff> fs', '<Esc>tt<kOn> tt', '<Esc>tn<kOn> tn', 'to', '<Esc>tz tz', '<Esc>tw tw']
"let b:qbar_00 = ['<Esc>fs<kOff> fs', '<Esc>tl<kOn> tl', '<Esc>tx tx', '<Esc>tw tw', '<Esc>ggta<kOn> ta', '<Esc>tt<kOn> tt', '<Esc>tn<kOn> tn', 'to', 'ti']

let b:lastHourUpdate = 0

let s:signHour = 21000
let s:cursorUpdateInterval = 5
let s:dmFormat = '%m/%d'
let s:dmyFileFormat = '%y%m%d'
let s:rxTime = '\(\(\d\{2\}\):\(\d\{2\}\)\s\)'
let s:rxDate = '\(\(\d\{2\}\)\/\(\d\{2\}\)\s\)'

let s:handlers = ['gnome-open', 'kde-open', 'exo-open', 'xdg-open']

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
function! s:AddToDate(dt, item, value) " 1
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

function! s:SubstituteTemplates(lines, dt) " 1
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

"Loads file by relative path
function! s:LoadFile(name) " 1
	let tmplfile = expand('%:h').'/'.a:name
	if !filereadable(tmplfile)
		echom 'File '.tmplfile.' is not readable'
		return []
	endif
	return readfile(tmplfile)
endfunction

"Returns indent of line
function! s:Indent(line) " 1
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

function! Insert_Template(clear) " 1
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

function! s:BufferSearchForLastChild(idx) " 1
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

function! Select_Tree() " 1
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

fun! Auto_Save() " 1
	silent! wa
endf

function! s:Load() " 1
	let rexp = '^\/\/ ttt:\(.\+\)$'
	au! * <buffer>
	normal mx
	call cursor(0, 0)
	let linenr = -1
	while linenr != 0
		let linenr = search(rexp, 'cWze')
		if linenr > 0
	 		let vars = s:ParseAttrs(getline(linenr), len('// ttt:'))
	 		for [var, value] in items(vars)
	 			exe 'let b:'.var.'='.value
	 		endfor
		endif
	endwhile
	setlocal foldlevel=0
	if exists('g:tttNoNumber') && g:tttNoNumber == 1
		setlocal nonumber
	endif
	normal `x
	if b:jump == 'b'
		normal Gzz
	endif
	if exists('g:tttAutoSave') && g:tttAutoSave == 0 || exists('b:as') && b:as == 0
		" Do nothing, no autosave
	else
		au CursorHold,InsertLeave * nested call Auto_Save()
	endif
endfunction

function! Make_Archive(addDate) " 1
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
function! s:MakeIndent(num, ...) " 1
	let str = ''
	let i = 0
	let fill = a:0 > 0 ? a:1 : "\t" 
	while i<a:num
		let str .= fill
		let i += 1
	endwhile
	return str
endfunction

function! Copy_Tree(indent) " 1
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

let s:signTask = 22000

fun! EndOfIndentBuffer(from, to)
	let indent = s:Indent(getline(a:from))
	let i = a:from + 1
	while i <= a:to
		let ind = s:Indent(getline(i))
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

let s:rxLine = '^\(\t*\)\(\S\)\(!\{1,5}\)\?\s\(.*\)$'

fun! ParseLine(text)
	let m = matchlist(a:text, s:rxLine)
	if len(m)>0
		return 
			\{
				\'type': m[2],
				\'priority': len(m[3])
			\}
	endif
	return 
		\{
			\'type': '',
			\'priority': 0
		\}
endf

setlocal foldtext=Fold_Text()

function! Fold_Text() " 1
	let nl = v:foldend - v:foldstart + 1
	let line = getline(v:foldstart)
	let m = matchlist(line, '^\s*\(.*\)\s/$')
	let indent = s:Indent(line)
	let res = s:MakeIndent(indent, "  ").m[1].'...{'.nl.'}'
	return res
endfunction

setlocal fillchars=fold:\ 

let maplocalleader = "t"

nnoremap <buffer> <silent><localleader>u :call ttt#Add_New_Line(0, '-', 1)<CR>
nnoremap <buffer> <silent><localleader>n :call ttt#Add_New_Line(0, '-', 0)<CR>
nnoremap <buffer> <silent><localleader>t :call ttt#Add_New_Line(2, "\t-", -1)<CR>
nnoremap <buffer> <silent><localleader>l :call ttt#Append_Log()<CR>
nnoremap <buffer> <silent><localleader>y :call Insert_Template(1)<CR>
nnoremap <buffer> <silent><localleader>a :call Insert_Template(0)<CR>
nnoremap <buffer> <silent><localleader>c :call Make_Archive(1)<CR>
nnoremap <buffer> <silent><localleader>s :call Select_Tree()<CR>
nnoremap <buffer> <silent><localleader>r :call Copy_Tree(1)<CR>

nnoremap <buffer> <silent><localleader>1 :call ttt#changeSign('-')<CR>
nnoremap <buffer> <silent><localleader>2 :call ttt#changeSign('=')<CR>
nnoremap <buffer> <silent><localleader>3 :call ttt#changeSign('#')<CR>
nnoremap <buffer> <silent><localleader>4 :call ttt#changeSign('/')<CR>
nnoremap <buffer> <silent><localleader>5 :call ttt#changeSign('~')<CR>

call s:Load()
call s:Enable_Markers()

