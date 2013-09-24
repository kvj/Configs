let s:okTags = ['ok', 'list']
let s:extBlockBegin = '#begin'

"syn match tTitle '^\s*\*\{1,4\}'
syn match tTime '\s:\d\{2}'hs=s+1
syn match tTime '\d\{2}:\d\{2}'
"syn match tDuration '\s*-\s*\d\{1,2}\(\:\d\d\)\?\(h\|m\)'
syn match tComment '^\s*\/\{2}\s.*$'
syn match tMark '^\s*\(\!\|@\|#\|?\|-\|+\|>\|<\|\$\|\~\)'
syn match tDate '\(\s\|^\)\d\{2}\/\d\{2}\(\/\d\{2}\)\?'
syn match tTag '#[a-z0-9]\+'
syn match tContact '@[a-zA-Z0-9-_]\+'
exe 'syn match tOkTag ''#\('.join(s:okTags, '\|').'\)\(\s\|$\)'''
syn match tPickTag '#\(achieve\|pick\)\(\s\|$\)'
syn match tTagInfo '\s-[a-z0-9]\+'hs=s+1
syn match tTitle '^\s*[A-Z0-9].*:$'he=e-1
syn match tTitle '^\s*[A-Z0-9].*: /'he=e-3
syn cluster BlockHL contains=tTagInfo,tPickTag,tContact
let markIndentBlock = ' skip=''^\z1\s'' end=''^\s*[^\s]''me=s-1'
exe 'syn region tOkLine start=''^\z(\s*\).*\s#\('.join(s:okTags, '\|').'\)\($\| \?/\?\)'''.markIndentBlock.' contains=@BlockHL'
"syn cluster ExtBlockSyntax contains=tBegin

exe 'syn region tExtBlock start=''^\z(\s*\)'.s:extBlockBegin.' '''.markIndentBlock.' keepend contains=@ExtBlockSyntax'

function! s:enableExtBlockSyntax(code, name)
	let ft=toupper(a:code)
	let group='ExtSyntax'.ft
	execute 'syntax include @'.group.' syntax/'.a:name.'.vim'
	execute 'syntax region tExtGroup'.ft.' matchgroup=tExtGroupMark start=''^\s*'.s:extBlockBegin.'\(\s\|\n\)'.a:code.'\s.*\n''rs=e skip=''\n'' end=''$'' contained contains=@'.group
	execute 'syn cluster ExtBlockSyntax add=tExtGroup'.ft
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
hi link tTag Todo
hi link tContact String
hi link tPickTag Type
hi link tTagInfo LineNr
hi link tOkLine Comment
hi link tBegin Question
hi link tOkTag Statement
hi link tBlockStartEnd String
"hi link tExtGroupMark Comment
