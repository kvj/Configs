exec 'source '.globpath(g:vim_config, 'bundle/pathogen.vim')

if exists('g:vimLite')
	let g:pathogen_disabled = ['airline', 'coffee', 'editorconfig', 'vim-go', 'vim-less', 'vimwiki', 'fugitive', 'taglist']
else
	let g:pathogen_disabled = ['']
endif

execute pathogen#infect(globpath(g:vim_config, 'bundle').'/{}')

set termencoding=utf-8
set fileencodings=utf-8
set encoding=utf-8
if !exists('g:vimLang')
	let g:vimLang = 'C'
endif
exec 'normal language messages '.g:vimLang
exec 'normal language ctype '.g:vimLang
exec 'normal language time '.g:vimLang
set langmenu=en_US.UTF-8
set autoread
set mouse=a
set wildmenu
set magic
set more
set cmdheight=1
set noerrorbells
set vb t_vb=
set showmatch
set mat=1
set nobackup
set nowb
set noswapfile
set noea
set lazyredraw
set modeline
set modelines=5
"set t_Co=256
set smartindent
set laststatus=3
set guicursor=a:blinkon300-blinkoff200
set autoread
set timeout timeoutlen=500 ttimeoutlen=100
"cc=80
set colorcolumn=100
set sessionoptions-=options

set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
if exists('g:vimLite')
	set statusline=%1*%m%*%2*%r%*%f%=\ Col:%3*%03c%*x%3*%03l%*/%3*%03L%*\ %3*%{&filetype}%*/%3*%{&fileformat}%*/%3*%{&fileencoding}%*%<
endif
set listchars=tab:>.,trail:~,extends:*
set nolist

let g:showmarks_enable = 1
hi! link ShowMarksHLl LineNr
hi! link ShowMarksHLu LineNr
hi! link ShowMarksHLo LineNr
hi! link ShowMarksHLm LineNr

"Wrap too long lines
set wrap
" Tabs are 4 characters
set tabstop=4

" (Auto)indent uses 4 characters
set shiftwidth=4

" guess indentation
set autoindent

" Expand the command line using tab
set wildchar=<Tab>

" show line numbers
set number

" Fold using markers {{{
" like this
" }}}
set foldmethod=indent
"set foldnestmax=3
"set foldminlines=5
set foldcolumn=2
set foldlevel=99

" enable all features
set nocompatible
" powerful backspaces
set backspace=indent,eol,start

" highlight the searchterms
set hlsearch

" jump to the matches while typing
set incsearch

" ignore case while searching
set smartcase

" don't wrap words
set textwidth=0

" history
set history=5000

" 10000 undo levels
set undolevels=10000

" show a ruler
set ruler

" show partial commands
set showcmd

" show matching braces
set showmatch

" write before hiding a buffer
set autowrite

" allows hidden buffers to stay unsaved, but we do not want this, so comment
" it out:
set hidden

"set wmh=0

" auto-detect the filetype
filetype plugin indent on
set ofu=syntaxcomplete#Complete
" syntax highlight
syntax on

" we use a dark background, don't we?
set bg=dark

" Always show the menu, insert longest match
set completeopt=menuone,preview,longest

let Tlist_GainFocus_On_ToggleOpen = 1
let Tlist_Use_Right_Window = 1
let Tlist_Show_One_File = 1
let Tlist_File_Fold_Auto_Close = 1
let Tlist_Auto_Update = 1
let Tlist_Highlight_Tag_On_BufEnter = 1
let Tlist_Enable_Fold_Column = 0
let Tlist_Close_On_Select = 0
let g:proj_window_width = 25
let g:proj_window_increment = 35
let g:bufExplorerShowRelativePath = 1
let g:proj_flags = "imsStTvg"
let g:proj_after_open = 'setlocal foldlevel=1'

let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplTabWrap = 1
let g:miniBufExplModSelTarget = 1
let g:miniBufExplUseSingleClick = 1
let g:miniBufExplMaxSize = 1
let g:NERDTreeShowBookmarks = 1
let g:NERDTreeQuitOnOpen = 1

"Eclim stuff
let g:VerticalToolWindowSide = 'right'
let g:EclimJavaHierarchyDefaultAction = 'edit'

if !exists('g:tttHotKeys')
	let g:tttHotKeys = {'00_Journal.ttt': 'x'}
endif

if !exists('g:tttReportDefault')
	let g:tttReportDefault = 'Work'
endif
let tttCalLocation = 'ttt:Calendar/1*_*.ttt'
let tttProjLocation = 'ttt:Project/2*_*.ttt'
let tttIndexLocation = 'ttt:0*_*.ttt'
let g:tttReports = {
	\'Work': {
		\'parts': [{
			\'files': tttCalLocation,
			\'type': '-=*',
			\'calendar': 1,
			\'title': 'Today:'
			\}, {
			\'files': tttProjLocation,
			\'tags': '-home +pin',
			\'type': '=',
			\'title': 'Work tasks:'
			\}, {
			\'files': tttIndexLocation,
			\'title': 'Inbox:'
			\}
		\]
	\},
	\'Home': {
		\'parts': [{
			\'files': tttCalLocation,
			\'type': '-=*',
			\'calendar': 1,
			\'title': 'Today:'
			\}, {
			\'files': tttProjLocation,
			\'tags': '-work +pin',
			\'type': '=',
			\'title': 'Home tasks:'
			\}, {
			\'files': tttIndexLocation,
			\'title': 'Inbox:'
			\}
		\]
	\}
\}

set ssop-=options
set ssop-=folds

if has('gui_running')
    set guioptions-=T
    set guioptions-=m
    set lines=70
    set columns=200
    set vb t_vb="<Esc>|10f"
endif

" When editing a file, always jump to the last known cursor position.
" Don't do it when the position is invalid or when inside an event handler
" (happens when dropping a file on gvim).
autocmd BufReadPost *
  \ if line("'\"") > 0 && line("'\"") <= line("$") |
  \   exe "normal g`\"" |
  \ endif

let mapleader='f'

map Q gq

"New paste
nnoremap <silent><LEADER>v jmpPV`pk

"Save selection
vnoremap < <gv
vnoremap > >gv

"Copy
vnoremap y ymygv<ESC>

"Restore pos
nnoremap bb `mzz

"Complete line
"inoremap <C-L> <C-X><C-L>

"Save
nnoremap <silent><LEADER>s :wa<CR>

"TagList
nnoremap <silent><LEADER>o :TlistToggle<CR>

"Project
"nnoremap <silent><LEADER>p :Project<CR>

"Show open windows
nnoremap <silent><LEADER>w :BufExplorer<CR>

"Close win
nnoremap <silent><LEADER>q :q<CR>

"Close win#2
nnoremap <silent><LEADER>Q :qa<CR>

"Next tab
nnoremap <silent><LEADER>f :e #<CR>

"Toggle tree
nnoremap <silent><LEADER>t :NERDTreeToggle<CR>

"Repeat search
"nnoremap <C-A> :/<CR>
"inoremap <C-A> <C-O>:/<CR>
"vnoremap <C-A> <ESC>:/<CR>

"Reset switch
nnoremap <silent><LEADER>/ :silent noh<CR>
"inoremap <silent><LEADER>/ <C-O>:silent noh<CR>
"vnoremap <silent><LEADER>/ <ESC>:silent noh<CR>

"Top win
nnoremap <C-I> :wincmd k<CR>
inoremap <C-I> <ESC>:wincmd k<CR>
vnoremap <C-I> <ESC>:wincmd k<CR>

"Bottom win
nnoremap <C-K> :wincmd j<CR>
inoremap <C-K> <ESC>:wincmd j<CR>
vnoremap <C-K> <ESC>:wincmd j<CR>

"Next win
nnoremap <C-L> :wincmd l<CR>
inoremap <C-L> <ESC>:wincmd l<CR>
vnoremap <C-L> <ESC>:wincmd l<CR>

"Prev win
nnoremap <C-J> :wincmd h<CR>
inoremap <C-J> <ESC>:wincmd h<CR>
vnoremap <C-J> <ESC>:wincmd h<CR>

"Win size
nnoremap <C-U> :vertical resize -1<CR>
"nnoremap <C-4> :vertical resize +1<CR>
nnoremap <C-O> :resize -1<CR>
"nnoremap <C-3> :resize +1<CR>

"Reload buffer
nnoremap <silent><LEADER>e :e<CR>
nnoremap <silent><LEADER>r :redr!<CR>

"Next tab
"nnoremap <C-TAB> :bNext<CR>
"inoremap <C-TAB> <C-O>:bNext<CR>
"vnoremap <C-TAB> <ESC>:bNext<CR>

"Alternative make
"nnoremap <silent><LEADER>. :make! alt<CR>:cl<CR>
"inoremap <silent><LEADER>. <ESC>:make! alt<CR>:cl<CR>
"vnoremap <silent><LEADER>. <ESC>:make! alt<CR>:cl<CR>

"Make
nnoremap <silent><LEADER>. :make!<CR>:cl<CR>
"inoremap <silent><LEADER>m <ESC>:make!<CR>:cl<CR>
"vnoremap <silent><LEADER>m <ESC>:make!<CR>:cl<CR>

"Next error
nnoremap <silent><LEADER>; :cn<CR>
"inoremap <silent><LEADER>; <C-O>:cn<CR>
"vnoremap <silent><LEADER>; <ESC>:cn<CR>

"Prev error
nnoremap <silent><LEADER>' :cp<CR>
"inoremap <silent><LEADER>' <C-O>:cp<CR>
"vnoremap <silent><LEADER>' <ESC>:cp<CR>

"Errors list
nnoremap <silent><LEADER>l :cl<CR>
"inoremap <silent><LEADER>l <ESC>:cl<CR>
"vnoremap <silent><LEADER>l <ESC>:cl<CR>

"Make result
nnoremap <silent><LEADER>n :cw<CR>
"inoremap <silent><LEADER>n <ESC>:cw<CR>
"vnoremap <silent><LEADER>n <ESC>:cw<CR>

"Select All
nnoremap <silent><LEADER>a ggVG
"inoremap <silent><LEADER>a <ESC>ggVG
"vnoremap <silent><LEADER>a <ESC>ggVG

"System clipboard
vnoremap <silent><LEADER>cy "+y
nnoremap <silent><LEADER>cp "+p
vnoremap <silent><LEADER>cd "+d

"Folding
nnoremap <silent><LEADER>z, zm
nnoremap <silent><LEADER>z. zr
nnoremap <silent><LEADER>za zO
nnoremap <silent><LEADER>zc zC
nnoremap <silent><LEADER>z1 :setlocal foldlevel=0<CR>
nnoremap <silent><LEADER>z2 :setlocal foldlevel=1<CR>
nnoremap <silent><LEADER>z3 :setlocal foldlevel=2<CR>
nnoremap <silent><LEADER>z4 :setlocal foldlevel=3<CR>

nnoremap <silent><LEADER>x :mksession! ~/.s.vim<CR>:qa<CR>
" nnoremap <silent><LEADER>n :mksession! ~/.s.vim<CR>
nnoremap <silent><LEADER>m :source ~/.s.vim<CR>

function! GitPullPush(cmd)
	let repo = fugitive#head()
	let origin = 'origin'
	let remote = input('['.a:cmd.']Remote? ', origin)
	let branch = input('['.a:cmd.']Branch? ', repo)
	exe ''.a:cmd.' '.remote.' '.branch
endfunction

nnoremap <silent><LEADER>gs :Gstatus<CR>
nnoremap <silent><LEADER>gu :call GitPullPush('Git push')<CR>
nnoremap <silent><LEADER>gf :call GitPullPush('Git pull')<CR>
nnoremap <silent><LEADER>gt :!tig status<CR>
nnoremap <silent><LEADER>gl :!tig<CR>

"source ~/.vimrc.local
