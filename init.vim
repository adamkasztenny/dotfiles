" - For Neovim: ~/.local/share/nvim/plugged
" - Avoid using standard Vim directory names like 'plugin'
call plug#begin('~/.vim/plugged')
set nocompatible              " :/be iMproved, required
filetype off                  " required
set encoding=utf8

" NERDTree stuff
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin'

" AWESOME search file
Plug 'junegunn/fzf',        { 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Colors
Plug 'lilydjwg/colorizer'
" CtrlP: Full path fuzzy finder
Plug 'ctrlpvim/ctrlp.vim'

Plug 'vim-airline/vim-airline-themes'
Plug 'vim-airline/vim-airline'
Plug 'flazz/vim-colorschemes'
Plug 'ryanoasis/vim-devicons'
set t_Co=256
" Editing stuff with magic

Plug 'tpope/vim-repeat'
Plug 'mbbill/undotree',             { 'on': 'UndotreeToggle'   }
Plug 'tpope/vim-surround'
Plug 'tpope/vim-endwise'
Plug 'junegunn/vim-after-object'
Plug 'kien/rainbow_parentheses.vim'
Plug 'junegunn/vim-fnr'
Plug 'majutsushi/tagbar'
Plug 'xolox/vim-easytags'
Plug 'xolox/vim-misc'

Plug 'Yggdroot/indentLine'

" Git
Plug 'tpope/vim-fugitive'
Plug 'mhinz/vim-signify'
Plug 'junegunn/gv.vim'

"Python
Plug 'nvie/vim-flake8'

"Auto complete 
Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
" Use deoplete.
let g:deoplete#enable_at_startup = 1

Plug 'neomake/neomake'

" Color Theme
Plug 'morhetz/gruvbox'

"dash app
Plug 'rizzatti/dash.vim'

"Clojure Stuff
Plug 'clojure-vim/acid.nvim'

" Tagging is all the rage
Plug 'ludovicchabant/vim-gutentags'
Plug 'AndrewRadev/switch.vim'

" Initialize plugin system
call plug#end()

"" ==========================================================
"                          Basic Settings
"" ==========================================================
set guifont=DroidSansMono_Nerd_Font:h16
set background=dark
colorscheme gruvbox

syntax on                     " syntax highlighing
filetype on                   " try to detect filetypes
filetype plugin indent on     " enable loading indent file for filetype
set relativenumber            " Display relative line numbers
set numberwidth=2             " using only 2 column (and 2 space) while possible
set background=dark           " We are using dark background in vim
set title                     " show title in console title bar
set wildmenu                  " Menu completion in command mode on <Tab>
set wildmode=full             " <Tab> cycles between all matching choices.
set noswapfile                " Do not create swap files
set pastetoggle=<F7>

" Disable comment continuation on paste
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

"if the only window left is nerdtree then close
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Ignore these files when completing
set wildignore+=*.o,*.obj,.git,*.pyc
set wildignore+=eggs/**
set wildignore+=*.egg-info/**
" Disable menu bars
:set guioptions-=m  "remove menu bar
:set guioptions-=T  "remove toolbar
:set guioptions-=r  "remove right-hand scroll bar
:set guioptions-=L  "remove left-hand scroll bar
""  Moving Around/Editing
set cursorline              "  have a line indicate the cursor location
set ruler                   "  show the cursor position all the time
set nostartofline           "  Avoid moving cursor to BOL when jumping around
set virtualedit=block       "  Let cursor move past the last char in <C-v> mode
set scrolloff=3             "  Keep 3 context lines above and below the cursor
set backspace=2             "  Allow backspacing over autoindent, EOL, and BOL
set showmatch               "  Briefly jump to a paren once it's balanced
set nowrap                  "  don't wrap text
set linebreak               "  don't wrap textin the middle of a word
set autoindent              "  always set autoindenting on
set smartindent             "  use smart indent if there is no indent file
set tabstop=2               "  <tab> inserts 4 spaces
set shiftwidth=2            "  but an indent level is 2 spaces wide.
set softtabstop=2           "  <BS> over an autoindent deletes both spaces.
set expandtab               "  Use spaces, not tabs, for autoindent/tab key.
set shiftround              "  rounds indent to a multiple of shiftwidth
set matchpairs+=<:>         "  show matching <> (html mainly) as well
set foldmethod=indent       "  allow us to fold on indents
set foldlevel=99            "  don't fold by default
set clipboard=unnamed
set splitbelow
set splitright

"""" Messages, Info, Status
set ls=2                    " allways show status line
set vb t_vb=                " Disable all bells.  I hate ringing/flashing.
set t_vb =                  " Disable screen blink in first line
autocmd GUIEnter * set vb t_vb= " for your GUI

set noerrorbells
set confirm                 " Y-N-C prompt if closing with unsaved changes.
set showcmd                 " Show incomplete normal mode commands as I type.
set report=0                " : commands always print changed line count.
set shortmess+=a            " Use [+]/[RO]/[w] for modified/readonly/written.
set ruler                   " Show some info, even without statuslines.
set laststatus=2            " Always show statusline, even if only 1 window.
set statusline=%<[%n]\ %F\ %m%r%y\ %{exists('g:loaded_fugitive')?fugitive#statusline():''}\ %=%-14.(%l,%c%V%)\ %P

"Nerdtree git plugin
let g:NERDTreeIndicatorMapCustom = {
            \ "Modified"  : "✹",
            \ "Staged"    : "✚",
            \ "Untracked" : "✭",
            \ "Renamed"   : "➜",
            \ "Unmerged"  : "═",
            \ "Deleted"   : "✖",
            \ "Dirty"     : "✗",
            \ "Clean"     : "✔︎",
            \ "Unknown"   : "?"
            \}

highlight OverLength ctermbg=darkred ctermfg=white guibg=#660000
match OverLength /\%81v.\+/

""setup tagbar elixir
let g:tagbar_type_elixir = {
            \ 'ctagstype' : 'elixir',
            \ 'kinds' : [
            \ 'f:functions',
            \ 'functions:functions',
            \ 'c:callbacks',
            \ 'd:delegates',
            \ 'e:exceptions',
            \ 'i:implementations',
            \ 'a:macros',
            \ 'o:operators',
            \ 'm:modules',
            \ 'p:protocols',
            \ 'r:records'
            \ ]
            \ }

""" Map Keys
nnoremap <leader>q :q!<CR>
nnoremap <leader>s :FZF<CR>
nnoremap <leader>w :w!<CR>
nnoremap <leader>l <C-w>
nnoremap <leader>L <C-W>

" <F8> | Tagbar
inoremap <F8> <esc>:TagbarToggle<cr>
nnoremap <F8> :TagbarToggle<cr>
let g:tagbar_sort = 0

" <F10> | NERD Tree
nnoremap <F10> :NERDTreeToggle<cr>
nnoremap <leader>n :NERDTreeToggle<cr>
" jk | Escaping!
inoremap jk <Esc>
xnoremap jk <Esc>
cnoremap jk <C-c>

" Make Y behave like other capitals
nnoremap Y y$

" ----------------------------------------------------------------------------
" Buffers
" ----------------------------------------------------------------------------
nnoremap ]b :bnext<cr>
nnoremap [b :bprev<cr>

" ----------------------------------------------------------------------------
" Tabs
" ----------------------------------------------------------------------------
nnoremap ]t :tabn<cr>
nnoremap [t :tabp<cr>

" ----------------------------------------------------------------------------
" ?ii / ?ai | indent-object
" ?io       | strictly-indent-object
" ----------------------------------------------------------------------------
function! s:indent_len(str)
  return type(a:str) == 1 ? len(matchstr(a:str, '^\s*')) : 0
endfunction

"" ----------------------------------------------------------------------------
"" #gi / #gpi | go to next/previous indentation level
"" ----------------------------------------------------------------------------
function! s:go_indent(times, dir)
    for _ in range(a:times)
        let l = line('.')
        let x = line('$')
        let i = s:indent_len(getline(l))
        let e = empty(getline(l))

        while l >= 1 && l <= x
            let line = getline(l + a:dir)
            let l += a:dir
            if s:indent_len(line) != i || empty(line) != e
                break
            endif
        endwhile
        let l = min([max([1, l]), x])
        execute 'normal! '. l .'G^'
    endfor
endfunction
nnoremap <silent> gi :<c-u>call <SID>go_indent(v:count1, 1)<cr>
nnoremap <silent> gpi :<c-u>call <SID>go_indent(v:count1, -1)<cr>

" ----------------------------------------------------------------------------
" :Root | Change directory to the root of the Git repository
" ----------------------------------------------------------------------------
function! s:root()
    let root = systemlist('git rev-parse --show-toplevel')[0]
    if v:shell_error
        echo 'Not in git repo'
    else
        execute 'lcd' root
        echo 'Changed directory to: '.root
    endif
endfunction
command! Root call s:root()

" ----------------------------------------------------------------------------
" vim-fugitive
" ----------------------------------------------------------------------------
nmap     <Leader>g :Gstatus<CR>gg<c-n>
nnoremap <Leader>d :Gdiff<CR>
nnoremap <Leader>b :Gblame<CR>

" ----------------------------------------------------------------------------
" undotree
" ----------------------------------------------------------------------------
"let g:undotree_WindowLayout = 2
nnoremap <F9> :UndotreeToggle<CR>

" ============================================================================
" FZF {{{
" ============================================================================

" nnoremap <silent> <Leader><Leader> :Files<CR>
nnoremap <silent> <expr> <Leader><Leader> (expand('%') =~ 'NERD_tree' ? "\<c-w>\<c-w>" : '').":Files\<cr>"
nnoremap <silent> <Leader>C        :Colors<CR>
nnoremap <silent> <Leader><Enter>  :Buffers<CR>
nnoremap <silent> <Leader>ag       :Ag <C-R><C-W><CR>
nnoremap <silent> <Leader>AG       :Ag <C-R><C-A><CR>
nnoremap <silent> <Leader>`        :Marks<CR>

autocmd FileType javascript noremap <buffer>  <leader>f  :call JsBeautify()<cr>
autocmd FileType json noremap <buffer> <leader>fj  :call JsonBeautify()<cr>
autocmd FileType jsx noremap <buffer> <leader>f  :call JsxBeautify()<cr>
autocmd FileType html noremap <buffer> <leader>f  :call HtmlBeautify()<cr>
autocmd FileType css noremap <buffer> <leader>f  :call CSSBeautify()<cr>

function! s:fzf_statusline()
  " Override statusline as you like
  highlight fzf1 ctermfg=161 ctermbg=251
  highlight fzf2 ctermfg=23 ctermbg=251
  highlight fzf3 ctermfg=237 ctermbg=251
  setlocal statusline=%#fzf1#\ >\ %#fzf2#fz%#fzf3#f
endfunction
autocmd! User FzfStatusLine call <SID>fzf_statusline()

"Enable python 3
let g:python_host_prog='/usr/local/bin/python3'

"setup better color for rainbow parentheses
let g:rbpt_colorpairs = [
    \ ['brown',       'RoyalBlue3'],
    \ ['Darkblue',    'SeaGreen3'],
    \ ['darkgray',    'DarkOrchid3'],
    \ ['darkgreen',   'firebrick3'],
    \ ['darkcyan',    'RoyalBlue3'],
    \ ['darkred',     'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['brown',       'firebrick3'],
    \ ['gray',        'RoyalBlue3'],
    \ ['black',       'SeaGreen3'],
    \ ['darkmagenta', 'DarkOrchid3'],
    \ ['Darkblue',    'firebrick3'],
    \ ['darkgreen',   'RoyalBlue3'],
    \ ['darkcyan',    'SeaGreen3'],
    \ ['darkred',     'DarkOrchid3'],
    \ ['red',         'firebrick3'],
    \ ]

let g:rbpt_max = 16
let g:rbpt_loadcmd_toggle = 0
au VimEnter * RainbowParenthesesToggle
au Syntax * RainbowParenthesesLoadRound
au Syntax * RainbowParenthesesLoadSquare
au Syntax * RainbowParenthesesLoadBraces

"<Ctrl-X> -- cut (goto visual mode and cut)
imap <C-X> <C-O>vgG
vmap <C-X> "*x<Esc>i

"<Ctrl-C> -- copy (goto visual mode and copy)
imap <C-C> <C-O>vgG
vmap <C-C> "*y<Esc>i

"<Ctrl-A> -- copy all
imap <C-A> <C-O>gg<C-O>gH<C-O>G<Esc>
vmap <C-A> <Esc>gggH<C-O>G<Esc>i

"<Ctrl-V> -- paste
nm \\paste\\ "=@*.'xy'<CR>gPFx"_2x:echo<CR>
imap <C-V> x<Esc>\\paste\\"_s
vmap <C-V> "-cx<Esc>\\paste\\"_x

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

let g:airline_powerline_fonts = 1

" Set Tags
set autochdir
set tags+=./tags;

" Disable Arrows
"noremap <Up> <NOP>
"noremap <Down> <NOP>
"noremap <Left> <NOP>
"noremap <Right> <NOP>

" Unselect the search result
map <Leader><Space> :noh<CR>

" Use the clipboard of Mac OS
if has('mac')
  set clipboard=unnamed
end

" Borrowed from @skwp
if executable('ag')
  " Use ag in CtrlP for listing files. Lightning fast and respects .gitignore
  let g:ctrlp_user_command =
    \ 'ag %s --files-with-matches -g "" --ignore "\.git$\|\.hg$\|\.svn$"'

  " ag is fast enough that CtrlP doesn't need to cache
  let g:ctrlp_use_caching = 0
else
  " Fall back to using git ls-files if Ag is not available
  let g:ctrlp_custom_ignore = '\.git$\|\.hg$\|\.svn$'
  let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . --cached --exclude-standard --others']
endif

let g:ctrlp_working_path_mode = 0
let g:ctrlp_switch_buffer = 'e'

:nmap <silent> <leader>f <Plug>DashSearch

" When reading a buffer (after 1s), and when writing.
call neomake#configure#automake('rw')
let g:neomake_open_list = 2

" find merge conflict markers
nnoremap <silent> <leader>fc <ESC>/\v^[<=>]{7}( .*\|$)<CR>

let g:gutentags_cache_dir = '~/.tags_cache'
let g:switch_mapping = "-"
