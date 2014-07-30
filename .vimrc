" ==================================================
" {{{UTIL
" ==================================================
set nocompatible

filetype plugin indent on     " required!

" fixing delay on leaving insert-mode
set notimeout
set ttimeout
set ttimeoutlen=10

" Don't redraw while executing macros (good performance config)
set lazyredraw

" instant regex preview
set incsearch

" fu swapfiles
set noswapfile
set nobackup

" show matching brackets
set showmatch

" folding
set foldmethod=marker

set undofile
" maximum number of changes that can be undone
set undolevels=1000 
" maximum number lines to save for undo on a buffer reload
set undoreload=10000 

set undodir=~/.vim/undodir//

" cursor-zeile markieren
set cursorline

" tolle regex
set magic

" Use 256 colours (Use this setting only if your terminal supports 256 colours)
set t_Co=256
set t_ut=

set fillchars+=stl:\ ,stlnc:\

" dont syntax highlight extrem long lines...
set synmaxcol=300

" set antialias
set antialias

" wrapping words
set formatoptions+=t
" set textwidth=80
set linebreak
set nowrap

" auto read file when a file is changed from outside
set autoread

" utf-8
set encoding=utf8
set termencoding=utf-8
set fileencoding=utf-8

" turn off cursor blink in normal mode
set gcr=n:blinkon0

" normal OS clipboard interaction
set clipboard=unnamed           

" make Vim run moar smooth
set ttyfast

" smart backspace
set backspace=indent,eol,start

" Tab-stuff
set expandtab
set smarttab

" mouse in all modes
set mouse=a

" 1 tab == 4 spaces
set shiftwidth=4
set tabstop=4
" copy the indention from prev line
set autoindent
" auto indent in some files e.g. C-like
set smartindent

" min 5 zeilen unten und oben platz
set scrolloff=5

" Line numbers
" set relativenumber
set number

" Resize splits when the window is resized
au VimResized * :wincmd =

" dont conceal latex commands like textit
let g:tex_conceal=""
" }}}

" ==================================================
" {{{FUNCTIONS
" ==================================================
function! LightColorscheme()
    colorscheme solarized
    let g:solarized_termtrans=0
    let g:solarized_termcolors=256
    let g:solarized_contrast="high"
    let g:solarized_visibility="normal"
    set background=light
endfunction

function! DarkColorscheme()
    colorscheme Tomorrow-Night
    set background=dark
endfunction
" }}}

" ==================================================
" {{{KEY BINDINGS AND COMMANDS
" ==================================================

" Treat long lines as break lines (useful when moving around in them)
map j gj
map k gk

nnoremap <leader>dark :call DarkColorscheme()<Cr>
nnoremap <leader>light :call LightColorscheme()<Cr>

" save on double esc
map <Esc><Esc> :w<CR>

" Y for yank until last non whitespace char, like D
nnoremap Y yg_

" Create newlines without entering insert mode
nnoremap go o<Esc>k
nnoremap gO O<Esc>j

command! Dodaline silent :%s/\. [^$]/\.\r/g
nnoremap <leader>ddl :Dodaline<Cr>

nnoremap <leader>js :JSHint<Cr>
nnoremap <leader>p :CtrlPMixed<Cr>
nnoremap <silent> <leader>f gg=G``
nnoremap <silent> <leader>n :NERDTreeTabsToggle<Cr>
nnoremap <leader>under :set syntax=underscore_template<Cr>

" navigate throw tabs
nnoremap <S-h> gT
nnoremap <S-l> gt

" space fuer comandmode
nnoremap <space> :

" save as sudo
cabbrev w!! w !sudo tee % > /dev/null %

cabbrev so :source ~/.vimrc

" toggle starting case of last word
nnoremap <leader>u b~w 

" smooth scroll
noremap <silent> <c-u> :call smooth_scroll#up(&scroll, 10, 2)<CR>
noremap <silent> <c-d> :call smooth_scroll#down(&scroll, 10, 2)<CR>

" emacs keybindings for command mode
cnoremap <c-a> <home>
cnoremap <c-e> <end>

" Split line (sister to [J]oin lines)
" The normal use of S is covered by cc, so don't worry about shadowing it.
nnoremap S i<cr><esc>mwgk:silent! s/\v +$//<cr>:noh<cr>`w
nnoremap gS a<cr><esc>mwgk:silent! s/\v +$//<cr>:noh<cr>`w
" }}}

" ==================================================
" {{{VUNDLE
" ==================================================
filetype off
set rtp+=~/.vim/bundle/Vundle.vim
call vundle#begin()

" let Vundle manage Vundle
" required! 
Plugin 'gmarik/Vundle.vim'

" My bundles here:
"
" original repos on GitHub
Plugin 'tpope/vim-fugitive'
Plugin 'Lokaltog/vim-easymotion'
Plugin 'rstacruz/sparkup', {'rtp': 'vim/'}
Plugin 'tpope/vim-rails.git'
" Vim-scripts repos
Plugin 'L9'
" }}}

" ==================================================
" {{{MEINE BUNDELS 
" ==================================================
" get ALL the colorschemes
Plugin 'flazz/vim-colorschemes'
" toggle Cursor
Plugin 'jszakmeister/vim-togglecursor'
" Vim airline
Plugin 'bling/vim-airline'
" Tmuxline
Plugin 'edkolev/tmuxline.vim'
" Promptline
Plugin 'edkolev/promptline.vim'
" GitGutter for Vim
Plugin 'airblade/vim-gitgutter'
" Nerdtree
Plugin 'scrooloose/nerdtree'
" Nerdtree in all tabs
Plugin 'jistr/vim-nerdtree-tabs'
" ctrlp
Plugin 'kien/ctrlp.vim'
" improved sessions for vim
Plugin 'manuel-colmenero/vim-simple-session'
" Autocomplete
Plugin 'Valloric/YouCompleteMe'
" Tern for Vim - JS
Plugin 'marijnh/tern_for_vim'
" Snippets for UltiSnip
Plugin 'honza/vim-snippets'
" UltiSnips Autocomplete
Plugin 'SirVer/ultisnips'
" Latex-Plugin
Plugin 'LaTeX-Box-Team/LaTeX-Box'
" Matching Tags
Plugin 'Valloric/MatchTagAlways'
" syntax highlight for jade
Plugin 'digitaltoad/vim-jade'
" auto close brackets
Plugin 'raimondi/delimitmate'
" jshint
Plugin 'Shutnik/jshint2.vim'
" underscore template highlight
Plugin 'aaronj1335/underscore-templates.vim'
" scroll smooth
Plugin 'terryma/vim-smooth-scroll'
" show indentions
Plugin 'nathanaelkane/vim-indent-guides'
" conceal javascript
Plugin 'tyok/js-mask'
" show colors
Plugin 'ap/vim-css-color'

call vundle#end()
filetype plugin indent on
" }}}

" ==================================================
" {{{AIRLINE CONF
" ==================================================
set laststatus=2
let g:airline#extensions#tabline#enabled=1
let g:airline_powerline_fonts=1
let g:solarized_termcolors=256
let g:airline_theme='bubblegum'
" let airline present current session
let g:airline_section_b='%{session#statusline()}'
" }}}

" ==================================================
" {{{TMUXLINE CONF
" ==================================================
let g:tmuxline_preset={
            \'a'    : '#S',
            \'b'    : ['#(whoami)', '#(uptime | cut -d " " -f 3,4,5 | cut -d "," -f 1)'],
            \'c'    : '#(echo `cat /sys/devices/platform/smapi/BAT0/remaining_percent`%)', 
            \'win'  : ['#I #W'],
            \'cwin' : ['#I #W'],
            \'y'    : ['%R', '%a', '%Y'],
            \'z'    : '#H'}
let g:tmuxline_powerline_separators=1
" }}}

" ==================================================
" {{{PROMPTLINE CONF
" ==================================================
let g:promptline_theme='airline'
let g:promptline_preset={
            \'a' : [ promptline#slices#jobs() ],
            \'b' : [ '%T' ],
            \'c' : [ promptline#slices#cwd({ 'dir_limit': 2 }) ],
            \'z' : [ promptline#slices#git_status(), promptline#slices#vcs_branch()],
            \'warn' : [ promptline#slices#last_exit_code(), promptline#slices#battery() ]}
let g:promptline_powerline_symbols=1
" }}}

" ==================================================
" {{{COLORSCHEME
" ==================================================
" SyntaxHighlight
syntax enable
colorscheme Tomorrow-Night
set background=dark
" }}}

" ==================================================
" {{{SETTINGS FOR GVIM/VIM
" ==================================================
if has("gui_running")

    " Disable Toolbar in gvim
    set guioptions-=T
    " Disable MenuBar in gvim
    set guioptions-=m
    " Disable left and right scrollbar
    set guioptions-=r
    set guioptions-=L

    " set guifont=Droid\ Sans\ Mono\ for\ Powerline\ 8
    set guifont=Monaco\ for\ Powerline\ 9

    " Colorscheme
    " let g:solarized_termtrans=0
    " let g:solarized_termcolors=256
    " let g:solarized_contrast="high"
    " let g:solarized_visibility="normal"
    " colorscheme Tomorrow-Night
else

    " Colorscheme
    " let g:solarized_termtrans=0
    " let g:solarized_termcolors=256
    " let g:solarized_contrast="high"
    " let g:solarized_visibility="normal"
    " colorscheme Tomorrow-Night
endif
" }}}

" =================================================
" {{{FILETYPE SETTINGS
" ==================================================
autocmd FileType tex source ~/.vim/fileTypeSettings/tex.vim

autocmd FileType scss source ~/.vim/fileTypeSettings/css.vim
autocmd FileType css source ~/.vim/fileTypeSettings/css.vim
" }}}

" ==================================================
" {{{ULTISNIPS CONFIG
" ==================================================
let g:UltiSnipsSnippetDirectories=["~/.vim/bundle/vim-snippets/UltiSnips"]
let g:UltiSnipsJumpBackwardTrigger="<s-tab>"

function! g:UltiSnips_Complete()
    call UltiSnips#ExpandSnippetOrJump()
    if g:ulti_expand_or_jump_res == 0
        if pumvisible()
            return "\<C-N>"
        else
            return "\<TAB>"
        endif
    endif
    return ""
endfunction

function! g:UltiSnips_Reverse()
    call UltiSnips#JumpBackwards()
    if g:ulti_jump_backwards_res == 0
        return "\<C-P>"
    endif
    return ""
endfunction 

au BufEnter * exec "inoremap <silent> " . g:UltiSnipsJumpBackwardTrigger . " <C-R>=g:UltiSnips_Reverse()<cr>"
" }}}

" ==================================================
" {{{NERDTREE CONFIG
" ==================================================
let g:nerdtree_tabs_open_on_gui_startup=0
let g:nerdtree_open_open_on_console_startup=0

let g:nerdtree_tabs_open_on_new_tab=0
" }}}
