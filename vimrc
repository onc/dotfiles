" -*- coding: utf-8; -*-
" vim:set filetype=vim fileencoding=utf-8:

" =============================================================================
" {{{ BASIC SETTINGS
" =============================================================================
set nocompatible

filetype on
filetype plugin on
filetype indent on 

" ######## SYNTAX #############################################################
syntax enable
set background=dark

" ######## ENCODING ###########################################################
set encoding=utf8
set termencoding=utf-8
set fileencoding=utf-8

" ######## LINE NUMBERS #######################################################
" Line numbers
" set relativenumber
set number

" ######## FORMAT #############################################################
" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4
set softtabstop=4 
" copy the indention from prev line
set autoindent
" auto indent in some files e.g. C-like
set smartindent
" wrapping words
set formatoptions+=t
" no new line after 80 chars
set textwidth=0
" wrap long lines - only for display, no new lines!
set linebreak
set wrap
" wrap 5 chars before right window border
set wrapmargin=5
" Tab-stuff
set expandtab
set smarttab
" smart backspace
set backspace=indent,eol,start

" ######## SEARCH #############################################################
" instant regex preview
set incsearch
" show all search results
set hlsearch
" turn off wrapping while searching
set nowrapscan
" tolle regex
set magic
" better search
set smartcase
set ignorecase

" ######## VISUAL #############################################################
" Don't redraw while executing macros (good performance config)
set lazyredraw
" show matching brackets
set showmatch
" graphical menu for command mode autocomplete
set wildmenu
" min 5 zeilen unten und oben platz
set scrolloff=5
" disable folding
set nofoldenable 
" set antialias
set antialias 
" use tabs
set switchbuf=usetab
" make Vim run moar smooth
set ttyfast
" mouse in all modes
set mouse=a
" cursor-zeile markieren
set cursorline
" fixing delay on leaving insert-mode
set notimeout
set ttimeout
set ttimeoutlen=10
" show column number 80
set colorcolumn=80

" ######## LANG ###############################################################
set spelllang=en

" ######## FILESYSTEM #########################################################
" fu swapfiles
set noswapfile
set nobackup
" auto read file when a file is changed from outside
set autoread
" normal OS clipboard interaction
set clipboard=unnamed

" ######## KEYBINDINGS ########################################################
" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk
" save on double esc
map <Esc><Esc> :w<CR>
" Y for yank until last non whitespace char, like D
nnoremap Y yg_
" Create newlines without entering insert mode
nnoremap go o<Esc>k
nnoremap gO O<Esc>j
" navigate throw tabs
nnoremap <S-h> gT
nnoremap <S-l> gt
" space fuer comandmode
nnoremap <space> :
" save as sudo
cabbrev w!! w !sudo tee % > /dev/null %
" source vimrc
cabbrev so :source ~/.vimrc
" emacs keybindings for command mode
cnoremap <c-a> <home>
cnoremap <c-e> <end>
" turn off hightlighting on backspace
nnoremap <silent> <bs> :nohl<cr>
" open vimrc
nnoremap <leader>vim :tabnew ~/.vimrc<cr>
" }}}

" =============================================================================
" {{{ EXTENDED SETTINGS
" =============================================================================
" ######## MUTE VIM ###########################################################
" stfu vim
set noerrorbells visualbell t_vb=
autocmd GUIEnter * set visualbell t_vb=

" ######## VISUAL #############################################################
" show line-endings
" set list
" set showbreak=↪
set listchars=eol:¬
" set listchars=eol:↪
" set listchars=tab:▸\ ,eol:¬,extends:❯,precedes:❮
set fillchars+=stl:\ ,stlnc:\
" dont syntax highlight extrem long lines...
set synmaxcol=300
" turn off cursor blink in normal mode
set gcr=n:blinkon0
" Use 256 colours (Use this setting only if your terminal supports 256 colours)
set t_Co=256
set t_ut=
" Resize splits when the window is resized
autocmd VimResized * :wincmd =

" ######## FILESYSTEM #########################################################
set undofile
" maximum number of changes that can be undone
set undolevels=1000 
" maximum number lines to save for undo on a buffer reload
set undoreload=10000 
set undodir=~/.vim/undodir//

if !has('nvim')
  set viminfo+=n~/.vim/viminfo
endif
" }}}
