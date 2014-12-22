let s:okTags = ['ok', 'list']
let s:extBlockBegin = '#begin'
let s:markIndentBlock = ' skip=''^\z1\s'' end=''^\s*[^\s]''me=s-1'

syn match tTime '\d\{1,2}:\d\{2}'
syn match tComment '^\s*\/\{2}\s.*$'
syn match tDate '\d\{2}\/\d\{1,2}\/\d\{1,2}'
syn match tTag '#[a-z0-9]\+'
syn match tPriority '!\{1,5}\s'he=e-1
syn match tContact '@[A-Z][a-zA-Z0-9-_]*'hs=s+1
exe 'syn match tOkTag ''#\('.join(s:okTags, '\|').'\)\(\s\|$\)'''
syn match tTitle '^\s*[A-Z0-9].*:'he=e-1
syn cluster BlockHL contains=tTag,tContact,tTime,tDate,tPriority
"exe 'syn region tOkLine start=''^\z(\s*\).*\s#\('.join(s:okTags, '\|').'\)\($\| \?/\?\)'''.markIndentBlock.' contains=@BlockHL'
"exe 'syn region tDoneLine start=''^\z(\s*\)#^\s.\+'''.markIndentBlock.' contains=@BlockHL'
"syn cluster ExtBlockSyntax contains=tBegin

exe 'syn region tExtBlock start=''^\z(\s*\)'.s:extBlockBegin.' '''.s:markIndentBlock.' keepend contains=@ExtBlockSyntax'

fun! EnableLineSyntaxt(name, sign)
	exe 'syn region t'.a:name.'Line  start=''^\z(\s\+\)'.a:sign.'\!\{,5}\s.*$'''.s:markIndentBlock.' contains=@BlockHL'
endf

call EnableLineSyntaxt('Done', '#')
"call EnableLineSyntaxt('Progress', '=')
"call EnableLineSyntaxt('Normal', '-')
call EnableLineSyntaxt('Wait', '\~')
call EnableLineSyntaxt('Cancel', '/')
call EnableLineSyntaxt('Calendar', '\*')

function! s:enableExtBlockSyntax(code, name)
	if !filereadable('syntax/'.a:name.'.vim')
		return 0
	endif
	let ft=toupper(a:code)
	let group='ExtSyntax'.ft
	execute 'syntax include @'.group.' syntax/'.a:name.'.vim'
	execute 'syntax region tExtGroup'.ft.' matchgroup=tExtGroupMark start=''^\s*'.s:extBlockBegin.'\(\s\|\n\)'.a:code.'\s.*\n''rs=e skip=''\n'' end=''$'' contained contains=@'.group
	execute 'syn cluster ExtBlockSyntax add=tExtGroup'.ft
	return 1
endfunction

call s:enableExtBlockSyntax('puml', 'plantuml')
call s:enableExtBlockSyntax('js', 'javascript')

exec 'syn match tBegin ''\s*'.s:extBlockBegin.'\s''he=e-1 contained containedin=tExtGroupMark'
"syn cluster ExtBlockSyntax add=tBegin
hi link tTitle Title
hi link tDate Special
hi link tTime Identifier
"hi link tDuration Type
hi link tMark Keyword
hi link tComment Comment
hi link tTag String
hi link tContact PreProc
hi link tPriority Todo
hi link tOkLine Comment
hi link tDoneLine Type
hi link tCancelLine LineNr
hi link tProgressLine Normal
hi link tWaitLine Folded
hi link tBegin Question
hi link tOkTag Statement
hi link tBlockStartEnd String
"hi link tExtGroupMark Comment
