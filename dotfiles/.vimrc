" vim: filetype=vim

""
"" General
""

" Save undo history to file (enables undo between sessions):
set undofile

" Keep : command history
set history=500

" Save undo history to separate dir:
set undodir=~/.vim/tmp/undo//,/tmp//

" Save swap files to separate dir:
set directory=~/.vim/tmp/swap//,/tmp//

" Automatically read in external changes to unchanged files:
set autoread

" Show line,column number in the statusline:
set ruler

" Show line numbers:
set number

" Disable intro messages when starting vim without a file:
set shortmess+=I


""
"" Syntax
""

" Enable usage of :highlight command:
syntax enable

" Disable syntax highlighting on long lines:
set synmaxcol=200

" Enable plugin, indent settings and file type detection:
filetype plugin indent on

" Force POSIX shell syntax:
let g:is_posix=1

" Disable background color erase for tmux 256 color support:
let &t_ut=''


""
"" UI
""

" Allow backspacing over autoindent, line breaks and start of insert:
set backspace=indent,eol,start

" Incrementally highlight search matches:
set incsearch

" Highlight all search matches:
set hlsearch

" Ignore case when searching with all lowercase queries:
set smartcase

" Substitute all matches on a line:
set gdefault

" Disable all bells:
set noerrorbells
set novisualbell
set t_vb=

" Completion settings with consecurive presses of TAB:
" 1. Complete till longest common string
" 2. When more than one match, list all matches
" 3. Complete next full match. After last match, the original string is used.
set wildmode=longest,list,full

" Recursively search when using :find etc:
set path=.,**

" Keep some lines above and below cursor:
set scrolloff=3

" Show as much as possibly from lines that do not fit on screen:
set display+=lastline

" Allow placing the curser where there is no character in visual block mode:
set virtualedit+=block

" Highlight the line of the cursor only in insert mode:
autocmd InsertLeave * set nocursorline
autocmd InsertEnter * set cursorline

set notimeout
set ttimeout
set ttimeoutlen=10

set hidden

set nrformats-=octal

autocmd QuickFixCmdPost *grep* cwindow


""
"" Keybindings
""

" Disable movement with cursor keys:
nnoremap <up> <nop>
nnoremap <down> <nop>
nnoremap <left> <nop>
nnoremap <right> <nop>
inoremap <up> <nop>
inoremap <down> <nop>
inoremap <left> <nop>
inoremap <right> <nop>

" Disable man page lookup of word under cursor with K key:
nnoremap K <nop>

" Disable tab cycling keys:
nnoremap gT <nop>
nnoremap gt <nop>

" Move up/down by display lines when long lines wrap:
nnoremap j gj
nnoremap k gk

" Shorter bindings for split navigation:
nnoremap <C-h> <C-w>h
nnoremap <C-j> <C-w>j
nnoremap <C-k> <C-w>k
nnoremap <C-l> <C-w>l

nnoremap * *zzzv
nnoremap # #zzzv
nnoremap n nzzzv
nnoremap N Nzzzv


""
"" Text
""
set wrap
set colorcolumn=80
set linebreak
silent! set breakindent
set autoindent

autocmd BufNewFile,BufRead * highlight trail_space ctermbg=1
autocmd InsertEnter * highlight trail_space ctermbg=NONE
autocmd InsertLeave * highlight trail_space ctermbg=1
autocmd BufNewFile,BufRead * match trail_space /\s\+$/

set noexpandtab
set smarttab
set softtabstop=0
set tabstop=8
set shiftwidth=8

set foldmethod=indent
set foldnestmax=3
set nofoldenable

set formatoptions+=j

""
"" Languages
""
function Spaces(n)
	setl expandtab
	exec 'setl softtabstop='.a:n
	exec 'setl tabstop='.a:n
	exec 'setl shiftwidth='.a:n
	setl shiftround
endfunction

command! Spaces :call Spaces(4)

au FileType python call Spaces(4)
au FileType html call Spaces(4)
au FileType jinja.html call Spaces(4)
au FileType css call Spaces(4)
au FileType yaml call Spaces(2)
au FileType markdown call Spaces(4)
au FileType vimwiki call Spaces(4)

au FileType mail setlocal textwidth=72


""
"" Leader
""
let mapleader = ","

nnoremap <leader>b :ls<CR>
nnoremap <leader>p :bp<CR>
nnoremap <leader>n :bn<CR>
nnoremap <leader>d :bd<CR>
nnoremap <leader># :b#<CR>
nnoremap <leader>1 :1b<CR>
nnoremap <leader>2 :2b<CR>
nnoremap <leader>3 :3b<CR>
nnoremap <leader>4 :4b<CR>
nnoremap <leader>5 :5b<CR>
nnoremap <leader>6 :6b<CR>
nnoremap <leader>7 :7b<CR>
nnoremap <leader>8 :8b<CR>
nnoremap <leader>9 :9b<CR>
nnoremap <leader>0 :10b<CR>

nnoremap <leader>i :set invpaste<CR>

nnoremap <leader>e :e <C-R>=expand("%:p:h") . "/" <CR>

nnoremap <leader>c :noh<CR>

""
"" Plugins
""

" colorscheme
let &t_8f = "\<Esc>[38;2;%lu;%lu;%lum"
let &t_8b = "\<Esc>[48;2;%lu;%lu;%lum"
set background=light
set termguicolors
colorscheme github

" netrw
let g:netrw_liststyle=3
let g:netrw_banner=0
nmap - :E %:h/<CR>

" buftabline
let g:buftabline_show=1
let g:buftabline_numbers=1
let g:buftabline_indicators=1

" vim-picker
nmap <c-p> <Plug>PickerEdit

" nerdtree

let NERDTreeMinimalUI = 1
let NERDTreeShowHidden = 1
let NERDTreeIgnore = ['\.git$[[dir]]']

function NERDTreeSync()
	if exists("g:NERDTree") && g:NERDTree.IsOpen()
		NERDTreeFind
	endif
endfunction
autocmd BufWinEnter * :call NERDTreeSync()

" black
let g:black_linelength = 79
autocmd BufWritePre *.py execute ':Black'

" git commit
function GitCi(n)
	!git ci -am sync && git push
endfunction

command! GitCi :call GitCi(4)
nnoremap <leader>g :GitCi<CR>
