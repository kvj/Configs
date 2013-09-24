" Vim syntax file
" Language:     PlantUML
" Maintainer:   Anders Thøgersen <first name at bladre dot dk>
" Last Change:  03-Apr-2011
" Version:      0.2
" TODO:         There are some bugs, add << >>
"
if version < 600
  syntax clear
endif

"let b:current_syntax = "plantuml"

syntax sync minlines=100

setlocal isident+=@
setlocal isident+=#

syntax match plantumlPreProc /\%(^@startuml\|^@enduml\)\|!\%(include\|ifdef\|define\|endif\)\s*.*/ contains=plantumlDir
syntax region plantumlDir start=/\s\+/ms=s+1 end=/$/ contained

syntax keyword plantumlTypeKeyword namespace component package interface class interface enum object participant activity skinparam 
syntax keyword plantumlKeyword actor partition title activate as deactivate note left right top bottom of end
syntax keyword plantumlKeyword if then else endif

syntax keyword plantumlCommentTODO XXX TODO FIXME NOTE contained
syntax match plantumlColor /#[0-9A-Fa-f]\{6\}\>/

" Arrows - Differentiate between horizontal and vertical arrows
syntax match plantumlHorizontalArrow /\%([-\.]\%(|>\|>\|\*\|o\>\|\\\\\|\\\|\/\/\|\/\|\.\|-\)\|\%(<|\|<\|\*\|\<o\|\\\\\|\\\|\/\/\|\/\)[\.-]\)\%(\[[^\]]*\]\)\?/ contains=plantumlLabel
syntax match plantumlDirectedOrVerticalArrowLR /[-\.]\%(le\?f\?t\?\|ri\?g\?h\?t\?\|up\?\|\do\?w\?n\?\)\?[-\.]\%(|>\|>>\|>\|\*\|o\>\|\\\\\|\\\|\/\/\|\/\|\.\|-\)\%(\[[^\]]*\]\)\?/ contains=plantumlLabel
syntax match plantumlDirectedOrVerticalArrowRL /\%(<|\|<<\|<\|\*\|\<o\|\\\\\|\\\|\/\/\|\/\)[-\.]\%(le\?f\?t\?\|ri\?g\?h\?t\?\|up\?\|\do\?w\?n\?\)\?[-\.]\%(\[[^\]]*\]\)\?/ contains=plantumlLabel
syntax region plantumlLabel start=/\[/ms=s+1 end=/\]/me=s-1 contained contains=plantumlText
syntax match plantumlText /\%(\w\|\s\|[\.,;_-]\)\+/ contained

" Class
syntax region plantumlClass start=/{/ end=/\s*}/ contains=plantumlClassArrows,
\                                                         plantumlKeyword,
\                                                         @plantumlClassOp

syntax match plantumlClassPublic      /+\w\+/ contained
syntax match plantumlClassPrivate     /-\w\+/ contained 
syntax match plantumlClassProtected   /#\w\+/ contained 
syntax match plantumlClassPackPrivate /\~\w\+/ contained

syntax cluster plantumlClassOp contains=plantumlClassPublic,
\                                       plantumlClassPrivate,
\                                       plantumlClassProtected,
\                                       plantumlClassProtected,
\                                       plantumlClassPackPrivate

" Strings
syntax match plantumlSpecialString /\\n/ contained
syntax region plantumlString start=/"/ skip=/\\\\\|\\"/ end=/"/ contains=plantumlSpecialString
syntax region plantumlString start=/'/ skip=/\\\\\|\\'/ end=/'/ contains=plantumlSpecialString
syntax match plantumlComment /'[^']*$/ contains=plantumlCommentTODO
syntax region plantumlMultilineComment start=/\/'/ end=/'\// contains=plantumlCommentTODO

" Labels with a colon
syntax match plantumlColonLine /:[^:]\+$/ contains=plantumlText

" Activity diagram
syntax match plantumlActivityThing /([^)]*)/
syntax match plantumlActivitySynch /===[^=]\+===/

" Skinparam keywords
" Highlight
highlight default link plantumlCommentTODO Todo
highlight default link plantumlKeyword Keyword
highlight default link plantumlTypeKeyword Type
highlight default link plantumlPreProc PreProc
highlight default link plantumlDir Constant
highlight default link plantumlColor Constant
highlight default link plantumlHorizontalArrow Identifier
highlight default link plantumlDirectedOrVerticalArrowLR Special
highlight default link plantumlDirectedOrVerticalArrowRL Special
highlight default link plantumlLabel Special
highlight default link plantumlText Label
highlight default link plantumlClassPublic Structure
highlight default link plantumlClassPrivate Macro
highlight default link plantumlClassProtected Statement
highlight default link plantumlClassPackPrivate Function
highlight default link plantumlSpecialString Special
highlight default link plantumlString String
highlight default link plantumlComment Comment
highlight default link plantumlMultilineComment Comment
highlight default link plantumlColonLine Comment
highlight default link plantumlActivityThing Type
highlight default link plantumlActivitySynch Type
"highlight default link plantumlSkinparamKeyword Identifier

