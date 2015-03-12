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
" folding
set foldmethod=marker
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
set spelllang=de,en

" ######## FILESYSTEM #########################################################
" fu swapfiles
set noswapfile
set nobackup
" auto read file when a file is changed from outside
set autoread
" normal OS clipboard interaction
set clipboard=unnamedplus

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

" ######## FILETYPE SETTINGS ##################################################
autocmd Bufread,BufNewFile *.tex set filetype=tex
" Vim interprets .md as 'modula2'
autocmd Bufread,BufNewFile *.md set filetype=markdown 
autocmd Bufread,BufNewFile *.mail set filetype=mail 

" spell in tex
autocmd FileType tex setlocal spell
autocmd BufNewFile,BufRead *.tex setlocal spell

" javascript
autocmd FileType javascript setlocal shiftwidth=2 tabstop=2 expandtab

" mail
autocmd FileType mail setlocal textwidth=80 spell

" C
autocmd FileType c setlocal foldmethod=syntax
" }}}

" =============================================================================
" {{{ EXTENDED SETTINGS
" =============================================================================

" ######## COLORSCHEMES #######################################################
let g:gui_dark_colorscheme = "base16-monokai"
let g:gui_light_colorscheme = "solarized"

let g:term_dark_colorscheme = "Tomorrow-Night"
let g:term_light_colorscheme = "solarized"

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

" ######## FILETYPE SETTINGS ##################################################
" dont conceal latex commands like textit
let g:tex_conceal=""

" ######## FUNCTIONS ##########################################################
function! LightColorscheme()
    if has("gui_running")
        execute "colorscheme ".g:gui_light_colorscheme
    else 
        execute "colorscheme ".g:term_light_colorscheme
    endif

    set background=light
    call HiInterestingWordGroups()
endfunction

function! DarkColorscheme()
    if has("gui_running")
        execute "colorscheme ".g:gui_dark_colorscheme
    else
        execute "colorscheme ".g:term_dark_colorscheme
    endif

    set background=dark
    call HiInterestingWordGroups()
endfunction

" ######## KEYBINDINGS ########################################################
" switch between colorschemes
nnoremap <leader>dark :silent :call DarkColorscheme()<Cr>
nnoremap <leader>light :silent :call LightColorscheme()<Cr>
" Format
nnoremap <silent> <leader>f gg=G``
" Split line (sister to [J]oin lines)
" The normal use of S is covered by cc, so don't worry about shadowing it.
nnoremap S i<cr><esc>mwgk:silent! s/\v +$//<cr>:noh<cr>`w
nnoremap gS a<cr><esc>mwgk:silent! s/\v +$//<cr>:noh<cr>`w
" Source
vnoremap <leader>S y:execute @@<cr>:echo 'Sourced selection.'<cr>
nnoremap <leader>S ^vg_y:execute @@<cr>:echo 'Sourced line.'<cr>
" Show hightlight group of char under cursor
nnoremap <leader>H :echo "hi<" . synIDattr(synID(line("."),col("."),1),"name") . '> trans<'
            \ . synIDattr(synID(line("."),col("."),0),"name") . "> lo<"
            \ . synIDattr(synIDtrans(synID(line("."),col("."),1)),"name") . ">"<CR>
" set filetype to undescore_template
nnoremap <leader>under :set syntax=underscore_template<Cr>
" Switch to alternate file
map <C-Tab> :bnext<cr>
map <C-S-Tab> :bprevious<cr>

" ######## COLORSCHEME SETTINGS ###############################################
" solarized
let g:solarized_termtrans=0
let g:solarized_termcolors=256
let g:solarized_contrast="high"
let g:solarized_visibility="normal"

"Access colors present in 256 colorspace
let g:base16colorspace=256  
" }}}

" =============================================================================
" {{{ MY BUNDELS 
" =============================================================================

" install plug if not installed
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !mkdir -p ~/.vim/autoload
  silent !curl -fLo ~/.vim/autoload/plug.vim
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall
endif

call plug#begin('~/.vim/plugged')
" git for vim
Plug 'tpope/vim-fugitive'
" toggle Cursor
Plug 'jszakmeister/vim-togglecursor'
" Vim airline
Plug 'bling/vim-airline'
" GitGutter for Vim
Plug 'airblade/vim-gitgutter'
" Nerdtree
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeTabsToggle'}
" Comments
Plug 'scrooloose/nerdcommenter'
" Nerdtree in all tabs
Plug 'jistr/vim-nerdtree-tabs'
" ctrlp
Plug 'kien/ctrlp.vim'
" Autocomplete
" Plug 'Valloric/YouCompleteMe'
" Tern for Vim - JS
Plug 'marijnh/tern_for_vim', { 'for': 'javascript' }
" snippets for ultisnip
Plug 'honza/vim-snippets'
" UltiSnips Autocomplete
Plug 'SirVer/ultisnips'
" Latex-Plugin
Plug 'LaTeX-Box-Team/LaTeX-Box', { 'for': ['latex', 'tex'] }
" Matching Tags
Plug 'Valloric/MatchTagAlways'
" auto close brackets
Plug 'raimondi/delimitmate'
" jshint
Plug 'Shutnik/jshint2.vim', { 'for': 'javascript' }
" underscore template highlight
Plug 'aaronj1335/underscore-templates.vim'
" show indentions
Plug 'nathanaelkane/vim-indent-guides'
" show colors
Plug 'lilydjwg/colorizer'
" text-objects-user
Plug 'kana/vim-textobj-user'
Plug 'Julian/vim-textobj-brace'
" javascript
Plug 'einars/js-beautify'
Plug 'maksimr/vim-jsbeautify'
" ruby
Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
" syntastic
" Plug 'scrooloose/syntastic'
" vim surround
Plug 'tpope/vim-surround'

Plug 'godlygeek/tabular'

Plug 'plasticboy/vim-markdown'

Plug 'oblitum/rainbow'

" COLORSCHEMES
" base 16 colorscheme
Plug 'chriskempson/base16-vim'
" Tomorrow
Plug 'chriskempson/vim-tomorrow-theme'
" solarized
Plug 'altercation/vim-colors-solarized'

call plug#end()
" }}}

" =============================================================================
" {{{ PLUGIN SETTINGS
" =============================================================================

" ######## YOU COMPLETE ME ####################################################
let g:rainbow_active = 1
" blue, orange, green, purple, white
" let g:rainbow_guifgs = ['Function', 'Number', 'String', 'Define', 'Normal']
" let g:rainbow_ctermfgs = ['Function', 'Number', 'String', 'Define', 'Normal'] 
let g:rainbow_load_separately = [
    \ [ '*' , [['(', ')'], ['\[', '\]'], ['{', '}']] ],
    \ [ '*.tex' , [['(', ')'], ['\[', '\]']] ],
    \ [ '*.cpp' , [['(', ')'], ['\[', '\]'], ['{', '}']] ],
    \ [ '*.{html,htm}' , [['(', ')'], ['\[', '\]'], ['{', '}'], ['<\a[^>]*>', '</[^>]*>']] ],
    \ ]
let g:rainbow_guifgs = ['RoyalBlue3', 'DarkOrange3', 'DarkOrchid3', 'FireBrick']
let g:rainbow_ctermfgs = ['lightblue', 'lightgreen', 'yellow', 'red', 'magenta']

" ######## YOU COMPLETE ME ####################################################
let g:ycm_global_ycm_extra_conf = '~/.vim/ycm_extra_conf.py'
let g:ycm_register_as_syntastic_checker = 0

" ######## AIRLINE ############################################################
set laststatus=2
let g:airline#extensions#tabline#enabled=1
let g:airline_powerline_fonts=1
let g:solarized_termcolors=256
let g:airline_theme='tomorrow'

" ######## ULTISNIPS ##########################################################
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

" ######## NERDTREE ###########################################################
let g:nerdtree_tabs_open_on_gui_startup=0
let g:nerdtree_open_open_on_console_startup=0

let g:nerdtree_tabs_open_on_new_tab=1

" ######## SYNTASTIC ##########################################################
"  " C-Files
"  let g:syntastic_c_checkers = ['gcc']
"  " check headerfiles
"  let g:syntastic_c_check_header = 1
"  " dont check external libs
"  let g:syntastic_c_no_include_search = 1
"  " dont search include dirs like /usr/include
"  let g:syntastic_c_no_default_include_dirs = 1
"  " auto-refresh header-files
"  let g:syntastic_c_auto_refresh_includes = 1
"  " use gcc
"  let g:syntastic_c_compiler = 'gcc'

" ######## PLUGIN KEYBINDINGS #################################################
" javascript
autocmd FileType javascript nnoremap <buffer> <leader>jb :call JsBeautify()<cr>
" for html
autocmd FileType html nnoremap <buffer> <leader>jb :call HtmlBeautify()<cr>
" for css or scss
autocmd FileType css nnoremap <buffer> <leader>jb :call CSSBeautify()<cr>
" jshint
nnoremap <leader>js :JSHint<Cr>
" ctrl-p
nnoremap <leader>p :CtrlPMixed<Cr>
" nerdtree
nnoremap <silent> <leader>n :NERDTreeTabsToggle<Cr>

" ######## CTRL-SPACE SETTINGS ################################################
set hidden
let g:ctrlspace_load_last_workspace_on_start=1
let g:ctrlspace_save_workspace_on_exit=1
let g:airline_exclude_preview = 1

" ######## CTRL-SPACE SETTINGS ################################################
cnoremap %% <C-R>=expand('%:h').'/'<cr>
map <leader>F :CtrlP %%<cr>

let g:ctrlp_mruf_max = 10000
let g:ctrlp_max_files = 10000
let g:ctrlp_mruf_last_entered = 0
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_extensions = ['mixed']
let g:ctrlp_cmd = 'CtrlPMixed'

" Open list of buffers in CtrlP
nnoremap <leader>be :CtrlPBuffer<CR>

" Faster access to CtrlP's MRU list
cabbrev mr CtrlPMRUFiles<CR>
nnoremap <leader>mr :CtrlPMRUFiles<cr>

" }}}

" =============================================================================
" {{{ SETTINGS FOR GVIM/VIM
" =============================================================================
if has("gui_running")
    " Disable Toolbar in gvim
    set guioptions-=T
    " Disable MenuBar in gvim
    set guioptions-=m
    " Disable left and right scrollbar
    set guioptions-=r
    set guioptions-=L
    " font
    set guifont=Droid\ Sans\ Mono\ for\ Powerline\ 9
    " set guifont=Monaco\ for\ Powerline\ 9

    execute "colorscheme ".g:gui_dark_colorscheme
else
    execute "colorscheme ".g:term_dark_colorscheme
endif
" }}}

" =============================================================================
" {{{INTERESTING WORD
" =============================================================================
function! HiInterestingWord(n)
    " Save our location.
    normal! mz
    " Yank the current word into the z register.
    normal! "zyiw
    " Calculate an arbitrary match ID.  Hopefully nothing else is using it.
    let mid = 86750 + a:n
    " Clear existing matches, but don't worry if they don't exist.
    silent! call matchdelete(mid)
    " Construct a literal pattern that has to match at boundaries.
    let pat = '\V\<' . escape(@z, '\') . '\>'
    " Actually match the words.
    call matchadd("InterestingWord" . a:n, pat, 1, mid)
    " Move back to our original location.
    normal! `z
endfunction "

" Default Highlights
function! HiInterestingWordGroups()
    hi def InterestingWord1 term=standout ctermfg=15 ctermbg=1 guifg=White guibg=Red
    hi def InterestingWord2 term=reverse cterm=reverse gui=reverse 
    hi def InterestingWord3 term=reverse ctermfg=235 ctermbg=222 guifg=#303030 guibg=#f0c674
    hi def InterestingWord4 term=standout ctermfg=0 ctermbg=11 guifg=Black guibg=Yellow
    hi def InterestingWord5 term=bold,reverse cterm=reverse ctermfg=240 ctermbg=222 gui=reverse guifg=#5e5e5e guibg=#f0c674
    hi def InterestingWord6 term=reverse cterm=reverse ctermfg=240 ctermbg=250 gui=reverse guifg=#5e5e5e guibg=#c5c8c6
endfunction

" Mappings
nnoremap <silent> <leader>1 :call HiInterestingWord(1)<cr>
nnoremap <silent> <leader>2 :call HiInterestingWord(2)<cr>
nnoremap <silent> <leader>3 :call HiInterestingWord(3)<cr>
nnoremap <silent> <leader>4 :call HiInterestingWord(4)<cr>
nnoremap <silent> <leader>5 :call HiInterestingWord(5)<cr>
nnoremap <silent> <leader>6 :call HiInterestingWord(6)<cr>
nnoremap <silent> <leader>0 :call clearmatches()<cr>

call HiInterestingWordGroups()
" }}}

" =============================================================================
" {{{SWAPPING WINDOWS
" =============================================================================
function! MarkWindowSwap()
    let g:markedWinNum = winnr()
endfunction

function! DoWindowSwap()
    "Mark destination
    let curNum = winnr()
    let curBuf = bufnr( "%" )
    exe g:markedWinNum . "wincmd w"
    "Switch to source and shuffle dest->source
    let markedBuf = bufnr( "%" )
    "Hide and open so that we aren't prompted and keep history
    exe 'hide buf' curBuf
    "Switch to dest and shuffle source->dest
    exe curNum . "wincmd w"
    "Hide and open so that we aren't prompted and keep history
    exe 'hide buf' markedBuf 
endfunction

nmap <silent> <leader>mw :call MarkWindowSwap()<CR>
nmap <silent> <leader>sw :call DoWindowSwap()<CR>
" }}}
