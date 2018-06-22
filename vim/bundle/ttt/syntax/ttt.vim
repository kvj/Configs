let s:extBlockBegin = '#begin'
let s:markIndentBlock = ' skip=''^\z1\s'' end=''^\s*[^\s]''me=s-1'

syn match tTime '\d\{1,2}:\d\{2}' "_12:34
syn match tLocation '\s\[.\{-}\]'hs=s+1 "_[...]
syn match tComment '^\s*\/\{2}\s.*$' "//_...
syn match tDate '\(\d\{2}\/\)\?\d\{1,2}\/\d\{1,2}' "18/12/5
syn match tTag '#[a-z0-9]\+' "#test
syn match tPriority '\s[#=-?]!\{1,5}\s'he=e-1,hs=s+2 "_?2_
syn match tContact '@[A-Z][a-zA-Z0-9-_]*'hs=s+1 "@Marco
" exe 'syn match tOkTag ''#\('.join(s:okTags, '\|').'\)\(\s\|$\)'''
syn cluster BlockHL contains=tTag,tContact,tTime,tDate,tPriority,tLocation,tDoneLine,tCancelLine,tWaitLine,tTriageLine
syn match tTitle '^\s*[A-Z0-9].*:\( /\)\?$'he=e-1 contains=@BlockHL "TITLE: /

exe 'syn region tExtBlock start=''^\z(\s*\)'.s:extBlockBegin.' '''.s:markIndentBlock.' keepend contains=@ExtBlockSyntax'
exe 'syn region tFoldedBlock start=''^\z(\s*\).* /$'''.s:markIndentBlock.' keepend transparent fold contains=@BlockHL'

fun! EnableLineSyntaxt(name, sign)
	exe 'syn region t'.a:name.'Line  start=''^\z(\s\+\)'.a:sign.'\!\{,5}\s.*$'''.s:markIndentBlock.' contains=@BlockHL'
endf

call EnableLineSyntaxt('Done', '#')
"call EnableLineSyntaxt('Progress', '=')
"call EnableLineSyntaxt('Normal', '-')
call EnableLineSyntaxt('Wait', '\~')
call EnableLineSyntaxt('Cancel', '/')
call EnableLineSyntaxt('Calendar', '\*')
call EnableLineSyntaxt('Triage', '?')

hi link tTitle Title
hi link tDate Keyword
hi link tTime Keyword
hi link tComment Comment
hi link tLocation Folded
hi link tTag String
hi link tContact PreProc
hi link tPriority Todo
hi link tDoneLine Comment
hi link tCancelLine Type
hi link tWaitLine Folded
hi link tTriageLine Keyword
hi link tBegin Question
" hi link tOkTag Statement
hi link tBlockStartEnd String
