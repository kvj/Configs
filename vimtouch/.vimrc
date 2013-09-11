if exists('vim_config')
	exec 'set runtimepath+='.globpath(g:vim_config,'vimfiles').','.globpath(g:vim_config,'vimfiles/after')
	let g:snippets_dir=globpath(g:vim_config, 'vimfiles/snippets')
endif

set termencoding=utf-8
set fileencodings=utf-8
set encoding=utf-8
set langmenu=en_US.UTF-8
set autoread
set wildmenu
set magic
set more
"set cmdheight=2
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
set smartindent
set laststatus=2
set guicursor=a:blinkon300-blinkoff200
set timeout timeoutlen=500 ttimeoutlen=100
set list listchars=tab:>.,trail:~,extends:*

let g:showmarks_enable = 1
hi! link ShowMarksHLl LineNr
hi! link ShowMarksHLu LineNr
hi! link ShowMarksHLo LineNr
hi! link ShowMarksHLm LineNr

set foldcolumn=2
set foldlevel=99
set wrap
set tabstop=4
set shiftwidth=4
set autoindent
set wildchar=<Tab>
set nocompatible
set backspace=indent,eol,start
set hlsearch
set incsearch
set smartcase
set textwidth=0
set history=5000
set undolevels=10000
set ruler
set showcmd
set showmatch
set autowrite
set hidden
filetype plugin indent on
set ofu=syntaxcomplete#Complete
syntax on
set bg=dark
set completeopt=menuone,preview,longest

let g:bufExplorerShowRelativePath = 1

let g:miniBufExplMapWindowNavArrows = 1
let g:miniBufExplMapCTabSwitchBufs = 1
let g:miniBufExplTabWrap = 1
let g:miniBufExplModSelTarget = 1
let g:miniBufExplUseSingleClick = 1
let g:miniBufExplMaxSize = 1
let g:NERDTreeShowBookmarks = 1
let g:NERDTreeQuitOnOpen = 1

let g:tttHotKeys = {'00_Organizer/01.ttt': 'x', '00_Organizer/07.ttt': 'w', '01_Timeline/00.ttt': 'z'}

colorscheme solarized
let g:solarized_contrast = "high"
let mapleader='f'

map Q gq

"New paste
"nnoremap <C-P> :set paste<CR>.:set nopaste<CR>
inoremap <silent><LEADER>c <ESC>pa
nnoremap <silent><LEADER>c jmpPV`pk

"Save selection
vnoremap n <gv
vnoremap m >gv

"Copy
vnoremap y ymygv<ESC>

"Restore pos
nnoremap bb `mzz

"Save
nnoremap <silent><LEADER>s :wa<CR>

"Show open windows
nnoremap <silent><LEADER>w :BufExplorer<CR>

"Close win
nnoremap <silent><LEADER>q :close<CR>

"Reload buffer
nnoremap <silent><LEADER>e :e<CR>

"Next tab
nnoremap <silent><LEADER>f :e #<CR>

"Toggle tree
nnoremap <silent><LEADER>t :NERDTreeToggle<CR>

"Reset switch
nnoremap <silent><LEADER>r :silent noh<CR>

"Top win
nnoremap <C-I> :wincmd k<CR>

"Bottom win
nnoremap <C-K> :wincmd j<CR>

"Next win
nnoremap <C-L> :wincmd l<CR>

"Prev win
nnoremap <C-J> :wincmd h<CR>

"Win size
nnoremap <C--> :vertical resize -1<CR>
nnoremap <C-=> :vertical resize +1<CR>
nnoremap <C-_> :resize -1<CR>
nnoremap <C-+> :resize +1<CR>

"Select All
nnoremap <silent><LEADER>a ggVG

