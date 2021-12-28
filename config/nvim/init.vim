" init.vim

" (neo)vim configuration to explore vim's capabilities.

" Editor {{{

" Neovim may set most of these by default.  Regardless.
set incsearch
set hlsearch
set ignorecase
set smartcase
set wildmenu
set lazyredraw
set showmatch
set nocp
set number

" Cursor blinks in insert mode
let $NVIM_TUI_ENABLE_CURSOR_SHAPE=1

" Document settings
" Tab is 4 spaces
set ts=4 sw=4

" Folding
set foldlevelstart=10
set foldnestmax=10
set foldmethod=indent

" }}}

" Keymappings {{{

" Leader prefix key
nnoremap <space> <nop>
let mapleader = " "

" Move between windows
nnoremap <c-h> <c-w>h
nnoremap <c-j> <c-w>j
nnoremap <c-k> <c-w>k
nnoremap <c-l> <c-w>l

" }}}

" Plugin declarations {{{

let data_dir = has('nvim') ? stdpath('data') . '/site' : '~/.vim'
if empty(glob(data_dir . '/autoload/plug.vim'))
  silent execute '!curl -fLo '.data_dir.'/autoload/plug.vim --create-dirs  https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim'
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif

call plug#begin('~/.config/nvim/plugged')

" fuzzy finder
Plug 'nvim-lua/plenary.nvim'
Plug 'nvim-telescope/telescope.nvim'

" tree-sitter parser
Plug 'nvim-treesitter/nvim-treesitter', {'do': ':TSUpdate'}

" Jump to point in buffer
Plug 'ggandor/lightspeed.nvim'

" delimiter manipulator
Plug 'tpope/vim-surround'

" Repeat plugin commands
Plug 'tpope/vim-repeat'

" More [/] commands for moving between pairs
Plug 'tpope/vim-unimpaired'

" Org-mode
Plug 'nvim-orgmode/orgmode'

call plug#end()

" }}}

" Plugin configuration {{{

" Telescope fuzzy finder
nnoremap <leader>ff <cmd>Telescope find_files theme=ivy<cr>
nnoremap <leader>fb <cmd>Telescope file_browser theme=ivy<cr>
nnoremap <leader>sG <cmd>Telescope git_files theme=ivy<cr>
nnoremap <leader>sg <cmd>Telescope live_grep theme=ivy<cr>
nnoremap <leader>b <cmd>Telescope buffers theme=ivy<cr>
" Documentation
nnoremap <leader>hh <cmd>Telescope help_tags theme=ivy<cr>
nnoremap <leader>hk <cmd>lua require 'telescope.builtin'.keymaps(require('telescope.themes').get_ivy({}))<cr>
" Select paste from available registers akin to counsel-yank-pop
nnoremap <c-p> <cmd>lua require'telescope.builtin'.registers(require('telescope.themes').get_ivy({}))<cr>

" Org-mode setup
lua << EOF
local parser_config = require "nvim-treesitter.parsers".get_parser_configs()
parser_config.org = {
  install_info = {
    url = 'https://github.com/milisims/tree-sitter-org',
    revision = 'f110024d539e676f25b72b7c80b0fd43c34264ef',
    files = {'src/parser.c', 'src/scanner.cc'},
  },
  filetype = 'org',
}

require'nvim-treesitter.configs'.setup {
  -- If TS highlights are not enabled at all, or disabled via `disable` prop, highlighting will fallback to default Vim syntax highlighting
  highlight = {
    enable = true,
    disable = {'org'}, -- Remove this to use TS highlighter for some of the highlights (Experimental)
    additional_vim_regex_highlighting = {'org'}, -- Required since TS highlighter doesn't support all syntax features (conceal)
  },
  ensure_installed = {'org'}, -- Or run :TSUpdate org
}

require('orgmode').setup({
  org_agenda_files = {'~/Notes/*'},
  org_default_notes_file = '~/Notes/daily.org',
})
EOF

" }}}

" Use "za" to toggle fold visibility
" vim:foldmethod=marker:foldlevel=0
