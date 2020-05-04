""""""""""""""""""""""""""""""""""""""""""
" .__       .__  __         .__          "
" |__| ____ |__|/  |_ ___  _|__| _____   "
" |  |/    \|  \   __\\  \/ /  |/     \  "
" |  |   |  \  ||  |   \   /|  |  Y Y  \ "
" |__|___|  /__||__| /\ \_/ |__|__|_|  / "
"         \/         \/              \/  "
"         				 "
"                    Created by Tenn1518 "
""""""""""""""""""""""""""""""""""""""""""

" vim-plug {{{
call plug#begin('~/.vim/plugged')

" To get more information about these plugins, take a look at README.md

" vim-sensible
Plug 'tpope/vim-sensible'

" vim-fugitive depends on unite.vim
Plug 'Shougo/unite.vim' | Plug 'tpope/vim-fugitive'

" ctrlp.vim depends on ag.vim (in this configuration)
Plug 'rking/ag.vim' | Plug 'ctrlpvim/ctrlp.vim'

" NERDTree, replacement for vim's browser
Plug 'preservim/nerdtree'

" vim-airline and vim-airline themes
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Vim-bufferline
Plug 'bling/vim-bufferline'

" Syntastic
Plug 'scrooloose/syntastic'

" Solarized Colorscheme
Plug 'lifepillar/vim-solarized8'

" Better Bdelete and Bwipeout commands
Plug 'moll/vim-bbye'

call plug#end()
" }}}

" Set options {{{
set incsearch
set hlsearch
set ignorecase
set smartcase
set wildmenu
set lazyredraw
set showmatch
set nocp
set number
" set cursorline
set so=2
set backupdir=~/.config/nvim/backups
set directory=~/.config/nvim/swaps

" Set cursor as blinking in insert mode, and a square in normal mode
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1

" Set tab to 4 spaces
set ts=4 sw=4

if exists("%undodir")
	set undodir=~/.config/nvim/undo
endif

" Folding behaviour
set foldlevelstart=10
set foldnestmax=10
set foldmethod=indent

" Enable syntax highlighting
syntax enable

" Set background dark for solarized colorscheme
set background=dark

filetype on
filetype indent on

" Automatic commands
augroup comment
	autocmd!
	autocmd FileType javascript nnoremap <buffer> <localleader>c I//<esc>
	autocmd FileType python nnoremap <buffer> <localleader>c I#<esc>
	autocmd BufNewFile,BufRead *.vim nnoremap <buffer> <localleader>c I"<esc>
	autocmd BufNewFile,BufRead *.md setlocal filetype=markdown
	autocmd bufwritepost vimrc source $MYVIMRC
augroup END

" Open NERDTree automatically when no file is specified
autocmd StdinReadPre * let s:std_in=1
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif
" }}}

" Mappings {{{
" Set <leader> and <localleader>
nnoremap "'" <nop>
let mapleader = "'"
let g:mapleader = "'"
let maplocalleader = ","

" Faster save and exit
nnoremap <leader>w :wq<cr>

" Toggle relativenumber
nnoremap <leader>r :set relativenumber!<cr>

" Edit init.vim
nnoremap <leader>ev :vsplit $MYVIMRC<cr>
nnoremap <leader>ep :vsp ~/.zshrc<cr>
nnoremap <leader>ea :vsp ~/.zsh_aliases<cr>
nnoremap <leader>s :source $MYVIMRC<cr>

" Switch to normal mode
inoremap jk <ESC>
inoremap <ESC> <NOP>

" Make yanking an entire line much easier
nnoremap yl 0y$

" Cut function
nnoremap yd 0y$dd

" Treat wrapped lines as individual when moving vertically
nnoremap j gj
nnoremap k gk

" Disable search highlight when <Leader>+<Space> is pressed
nnoremap <silent> <leader><space> :noh<cr>

" Move between windows in normal mode
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l

" Buffer shortcuts
nnoremap <leader>x :Bdelete<cr>
nnoremap <leader>n :enew<cr>
nnoremap <leader>h :bprevious<cr>
nnoremap <leader>l :bnext<cr>

" Space toggles a fold
nnoremap <space> za

" The Silver Searcher mapping
nnoremap <leader>a :Ag

" Ctrl+P mapping
let g:ctrlp_map = "<c-p>"
let g:ctrlp_cmd = "CtrlP"

" NERDTree mapping
map <C-n> :NERDTreeToggle<CR>

" Fugitive git wrapper mappings
nnoremap <leader>ga :Gwrite<cr>
nnoremap <leader>gc :Gcommit<cr>
nnoremap <leader>gr :Gremove<cr>
" }}}

" Colorscheme {{{

set termguicolors
" Vim-specific sequences for RGB colors
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"

set background=dark
colorscheme solarized8

" }}}

" Vim-airline {{{
" Powerline symbols for vim-airline
let g:airline_powerline_fonts = 1

" Set vim-airline theme
" Comment the following line if you wish not to use the base16-solarized
" colorscheme
let g:airline_theme="solarized"

" Default vim-airline theme
" Uncomment the following line if you wish to use the default vim-airline
" theme
let g:airline_theme="dark"

" Enable the list of buffers
let g:airline#extensions#tabline#enabled = 1

" Show just the filename
let g:airline#extensions#tabline#fnamemod = ':t'
" }}}

" Vim-bufferline {{{

" Stop bufferline echoing to command bar
let g:bufferline_echo = 0

" }}}

" CtrlP {{{
let g:ctrlp_match_window = 'bottom,order:ttb'
let g:ctrlp_switch_buffer = 0
let g:ctrlp_working_path_mode = 0
let g:ctrlp_user_command = 'ag %s -l --nocolor --hidden -g ""'
" }}}

" Enable folding in init.vim to make editing init.vim easier
" Use "za" or "<space>" to open/close a tab
" vim:foldmethod=marker:foldlevel=0
