"syn match tTitle '^\s*\*\{1,4\}'
syn match tTime '\(\s\|\d\{2}\):\d\{2}'
"syn match tDuration '\s*-\s*\d\{1,2}\(\:\d\d\)\?\(h\|m\)'
syn match tComment '^\s*\/\{2}\s.*$'
syn match tMark '^\s*\(\!\|@\|#\|?\|-\|+\|>\|<\|\$\|\~\)'
syn match tDate '\(\s\|^\)\d\{2}\/\d\{2}\(\/\d\{2}\)\?'
syn match tTag '#[a-z0-9]\+'
syn match tContact '@[a-zA-Z0-9-_]\+'
syn match tOkTag '#\(ok\|moved\)\(\s\|$\)'
syn match tPickTag '#\(achieve\|pick\)\(\s\|$\)'
syn match tTagInfo '\s-[a-z0-9]\+'
syn match tTitle '^\s*[A-Z0-9].*:$'

hi link tTitle Title
hi link tDate Special
hi link tTime Identifier
"hi link tDuration Type
hi link tMark Keyword
hi link tComment Comment
hi link tTag Todo
hi link tContact String
hi link tOkTag Statement
hi link tPickTag Type
hi link tTagInfo LineNr

