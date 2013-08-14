"syn match tTitle '^\s*\*\{1,4\}'
syn match tDate '\d\{1,2}\/\d\{1,2}\(\/\d\{1,2}\)\?'
syn match tTime '\d\{1,2}\:\d\{2}'
"syn match tDuration '\s*-\s*\d\{1,2}\(\:\d\d\)\?\(h\|m\)'
syn match tComment '^\s*\/\{2}\s.*$'
syn match tMark '^\s*\(\!\|@\|#\|?\|-\|+\|>\|<\|\$\|\~\)\s'

"hi link tTitle Title
hi link tDate Special
hi link tTime Identifier
"hi link tDuration Type
hi link tMark Keyword
hi link tComment Comment

