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
Plug 'scrooloose/nerdtree'

" vim-colors-solarized
Plug 'altercation/vim-colors-solarized'

" vim-airline and vim-airline themes
Plug 'vim-airline/vim-airline'
Plug 'vim-airline/vim-airline-themes'

" Vim-bufferline
Plug 'bling/vim-bufferline'

" Syntastic
Plug 'scrooloose/syntastic'

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
set cursorline
set so=2
set backupdir=~/.config/nvim/backups
set directory=~/.config/nvim/swaps

" Set cursor as blinking in insert mode, and a square in normal mode
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1

if exists("%undodir")
	set undodir=~/.config/nvim/undo
endif

" Folding behaviour
set foldlevelstart=10
set foldnestmax=10
set foldmethod=indent

" Enable syntax highlighting
syntax enable

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

" Default file explorer to VimFiler
let g:vimfiler_as_default_explorer = 1
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

" Move around easier in insert mode
inoremap <c-h> <left>
inoremap <c-j> <down>
inoremap <c-k> <up>
inoremap <c-l> <right>

" Strong H/L
nnoremap H 0
nnoremap L $

" Move a line of text using ALT+[jk] or Command+[jk] on mac
nmap <M-k> mz:m-2<cr>`z
nmap <M-j> mz:m+<cr>`z
vmap <M-j> :m'>+<cr>`<my`>mzgv`yo`z
vmap <M-k> :m'<-2<cr>`>my`<mzgv`yo`z

" Disable the old way in order to force myself into learning the new way
nnoremap $ <nop>
nnoremap 0 <nop>
nnoremap ^ <nop>

" Switch to normal mode
inoremap jk <ESC>
inoremap <ESC> <NOP>

" Make yanking an entire line much easier
nnoremap yl 0y$

" Cut function
nnoremap yd 0y$dd

" Visual mode pressing * or # searches for the current selection
function! VisualSelection(direction, extra_filter) range
    let l:saved_reg = @"
    execute "normal! vgvy"

    let l:pattern = escape(@", '\\/.*$^~[]')
    let l:pattern = substitute(l:pattern, "\n$", "", "")

    if a:direction == 'b'
        execute "normal ?" . l:pattern . "^M"
    elseif a:direction == 'gv'
        call CmdLine("Ag \"" . l:pattern . "\" " )
    elseif a:direction == 'replace'
        call CmdLine("%s" . '/'. l:pattern . '/')
    elseif a:direction == 'f'
        execute "normal /" . l:pattern . "^M"
    endif

    let @/ = l:pattern
    let @" = l:saved_reg
endfunction
vnoremap <silent> * :call VisualSelection('f', '')<CR>
vnoremap <silent> # :call VisualSelection('b', '')<CR>

" Treat wrapped lines as individual when moving vertically
nnoremap j gj
nnoremap k gk

" Map <Space> to search and Ctrl+<Space> to search backwards
nnoremap <space> /
nnoremap <c-space> ?

" Disable search highlight when <Leader>+<Space> is pressed
nnoremap <silent> <leader><space> :noh<cr>

" Move between windows in normal mode
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l

" Close buffer
nnoremap <leader>bd :Bclose<cr>:tabclose<cr>gT

" Useful mappings for managing tabs
map <leader>tn :tabnew<cr>
map <leader>to :tabonly<cr>
map <leader>tc :tabclose<cr>
map <leader>tm :tabmove 
map <leader>t<leader> :tabnext

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
" Vim Solarized theme
" Uncomment the following lines if you wish not to use the solarized
" colorscheme
set background=dark
colorscheme solarized
" }}}

" Vim-airline {{{
" Powerline symbols for vim-airline
let g:airline_powerline_fonts = 1

" Set vim-airline theme
" Comment the following line if you wish not to use the solarized
" colorscheme
let g:airline_theme="solarized"

" Default vim-airline theme
" Uncomment the following line if you wish to use the default vim-airline
" theme
" let g:airline_theme="dark"
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
