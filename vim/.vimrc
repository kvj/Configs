if exists('vim_config')
	exec 'set runtimepath+='.globpath(g:vim_config,'vimfiles').','.globpath(g:vim_config,'vimfiles/after')
	let g:snippets_dir=globpath(g:vim_config, 'vimfiles/snippets')
endif

set termencoding=utf-8
set fileencodings=utf-8
set encoding=utf-8
set autoread
set mouse=
set wildmenu
set magic
set more
set cmdheight=3
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
set laststatus=2
set guicursor=a:blinkon300-blinkoff200
set autoread
set timeout timeoutlen=500 ttimeoutlen=100
"cc=80


"set statusline=%1*%m%*%2*%r%*%F%=\ Col:%3*%03c%*\ Ln:%3*%03l%*/%3*%03L%*\ File:%3*%{&filetype}%*/%3*%{&fileformat}%*/%3*%{&fileencoding}%*%<

set list listchars=tab:>.,trail:~,extends:*

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

" spaces instead of tabs
"set expandtab

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
"let g:miniBufExplorerMoreThanOne = 0
"let g:calendar_focus_today = 1
"let g:calendar_navi = ''
"let g:calendar_monday = 1
"let g:calendar_weeknm = 1
"let g:calendar_datetime = 'title'

"Eclim stuff
let g:VerticalToolWindowSide = 'right'
let g:EclimJavaHierarchyDefaultAction = 'edit'


colorscheme solarized

if has('gui_running')
    set guioptions-=T
    set guioptions-=m
    "colorscheme desertEx
    "set guifont=Droid\ Sans\ Mono:h10
    "set guifont=Ubuntu\ Mono:h10
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

let mapleader=','

map Q gq

"Edit .vimrc
nnoremap <silent><LEADER>v :e $MYVIMRC<CR>

"Reload .vimrc
nnoremap <silent><LEADER>s :source $MYVIMRC<CR>

"New paste
"nnoremap <C-P> :set paste<CR>.:set nopaste<CR>
inoremap  <ESC>pa
nnoremap  jmpPV`pk

"Save selection
vnoremap < <gv
vnoremap > >gv

"Copy
vnoremap y ymygv<ESC>

"Restore pos
nnoremap bb `mzz

"Complete line
inoremap <C-L> <C-X><C-L>

"nnoremap <silent><LEADER>3 :%s/>\s*</>\r</g<CR>ggVG=
"Save
nnoremap <silent><LEADER>2 :w<CR>
inoremap <silent><LEADER>2 <ESC>:w<CR>
vnoremap <silent><LEADER>2 <ESC>:w<CR>

"TagList
"nnoremap <silent><LEADER>o :TlistOpen<CR>
"inoremap <silent><LEADER>o <ESC>:TlistOpen<CR>
"vnoremap <silent><LEADER>o <ESC>:TlistOpen<CR>
nnoremap <silent><LEADER>o :TlistToo<CR>
inoremap <silent><LEADER>o <ESC>:TlistToo<CR>
vnoremap <silent><LEADER>o <ESC>:TlistToo<CR>

"Project
nnoremap <silent><LEADER>p :Project<CR>
inoremap <silent><LEADER>p <ESC>:Project<CR>
vnoremap <silent><LEADER>p <ESC>:Project<CR>

"Show open windows
nnoremap <silent><LEADER>w :BufExplorer<CR>
inoremap <silent><LEADER>w <ESC>:BufExplorer<CR>
vnoremap <silent><LEADER>w <ESC>:BufExplorer<CR>

"Close win
nnoremap <silent><LEADER>q :close<CR>
inoremap <silent><LEADER>q <C-O>:close<CR>
vnoremap <silent><LEADER>q <C-O>:close<CR>

"Close win#2
nnoremap <silent><LEADER>Q :q!<CR>
inoremap <silent><LEADER>Q <C-O>:q!<CR>
vnoremap <silent><LEADER>Q <C-O>:q!<CR>

"Next tab
nnoremap <silent><LEADER>, :e #<CR>
inoremap <silent><LEADER>, <ESC>:e #<CR>
vnoremap <silent><LEADER>, <ESC>:e #<CR>

"Toggle tree
nnoremap <silent><LEADER>t :NERDTreeToggle<CR>
inoremap <silent><LEADER>t <ESC>:NERDTreeToggle<CR>
vnoremap <silent><LEADER>t <ESC>:NERDTreeToggle<CR>

"Repeat search
"nnoremap <C-A> :/<CR>
"inoremap <C-A> <C-O>:/<CR>
"vnoremap <C-A> <ESC>:/<CR>

"Reset switch
nnoremap <silent><LEADER>/ :silent noh<CR>
inoremap <silent><LEADER>/ <C-O>:silent noh<CR>
vnoremap <silent><LEADER>/ <ESC>:silent noh<CR>

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
nnoremap <C--> :vertical resize -1<CR>
nnoremap <C-=> :vertical resize +1<CR>
nnoremap <C-_> :resize -1<CR>
nnoremap <C-+> :resize +1<CR>

"Next tab
"nnoremap <C-TAB> :bNext<CR>
"inoremap <C-TAB> <C-O>:bNext<CR>
"vnoremap <C-TAB> <ESC>:bNext<CR>

"Alternative make
"nnoremap <silent><LEADER>. :make! alt<CR>:cl<CR>
"inoremap <silent><LEADER>. <ESC>:make! alt<CR>:cl<CR>
"vnoremap <silent><LEADER>. <ESC>:make! alt<CR>:cl<CR>

"Make
"nnoremap <silent><LEADER>m :make!<CR>:cl<CR>
"inoremap <silent><LEADER>m <ESC>:make!<CR>:cl<CR>
"vnoremap <silent><LEADER>m <ESC>:make!<CR>:cl<CR>

"Next error
nnoremap <silent><LEADER>; :cn<CR>
inoremap <silent><LEADER>; <C-O>:cn<CR>
vnoremap <silent><LEADER>; <ESC>:cn<CR>

"Prev error
nnoremap <silent><LEADER>' :cp<CR>
inoremap <silent><LEADER>' <C-O>:cp<CR>
vnoremap <silent><LEADER>' <ESC>:cp<CR>

"Errors list
nnoremap <silent><LEADER>l :cl<CR>
inoremap <silent><LEADER>l <ESC>:cl<CR>
vnoremap <silent><LEADER>l <ESC>:cl<CR>

"Make result
nnoremap <silent><LEADER>n :cw<CR>
inoremap <silent><LEADER>n <ESC>:cw<CR>
vnoremap <silent><LEADER>n <ESC>:cw<CR>

"Select All
nnoremap <silent><LEADER>a ggVG
inoremap <silent><LEADER>a <ESC>ggVG
vnoremap <silent><LEADER>a <ESC>ggVG

"source ~/.vimrc.local
