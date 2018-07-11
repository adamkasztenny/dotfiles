call plug#begin('~/.vim/plugged')

" NERDTree stuff
Plug 'scrooloose/nerdtree', { 'on': 'NERDTreeToggle' }
Plug 'Xuyuanp/nerdtree-git-plugin'

" AWESOME search file
Plug 'junegunn/fzf',        { 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Colors
Plug 'lilydjwg/colorizer'
Plug 'vim-airline/vim-airline-themes'
Plug 'vim-airline/vim-airline'
Plug 'flazz/vim-colorschemes'
Plug 'arcticicestudio/nord-vim'
" Editing stuff with magic

Plug 'tpope/vim-repeat'
Plug 'mbbill/undotree',             { 'on': 'UndotreeToggle'   }
Plug 'tpope/vim-surround'
Plug 'tpope/vim-endwise'
Plug 'junegunn/vim-after-object'
Plug 'junegunn/vim-fnr'
Plug 'majutsushi/tagbar'

Plug 'Yggdroot/indentLine'

" Git
Plug 'tpope/vim-fugitive'
Plug 'mhinz/vim-signify'
Plug 'junegunn/gv.vim'

"lint
Plug 'w0rp/ale'

" Vim indent guides
Plug 'nathanaelkane/vim-indent-guides'

" let Vundle manage Vundle, required
Plug 'VundleVim/Vundle.vim'

Plug 'jiangmiao/auto-pairs'

"Go
Plug 'fatih/vim-go', { 'do': ':GoUpdateBinaries' }

"Terraform plugins
Plug 'hashivim/vim-terraform'
Plug 'vim-syntastic/syntastic'
Plug 'juliosueiras/vim-terraform-completion'

Plug 'kien/rainbow_parentheses.vim'

"Plug 'Shougo/deoplete.nvim'
"Plug 'roxma/nvim-yarp'
"Plug 'roxma/vim-hug-neovim-rpc'
Plug 'Valloric/YouCompleteMe'

"Code Snips
Plug 'SirVer/ultisnips'
Plug 'honza/vim-snippets'

call plug#end()

"" ==========================================================
"                          Basic Settings
"" ==========================================================
set guifont=DroidSansMono_Nerd_Font:h16
set anti enc=utf-8
"set guifont=Source\ Code\ Pro\ Bold\ 11

syntax on                     " syntax highlighing
filetype on                   " try to detect filetypes
filetype plugin indent on     " enable loading indent file for filetype
set relativenumber            " Display relative line numbers
set numberwidth=2             " using only 2 column (and 2 space) while possible
set title                     " show title in console title bar
set wildmenu                  " Menu completion in command mode on <Tab>
set wildmode=full             " <Tab> cycles between all matching choices.
set noswapfile                " Do not create swap files
set pastetoggle=<F7>

" Disable comment continuation on paste
autocmd FileType * setlocal formatoptions-=c formatoptions-=r formatoptions-=o

"if the only window left is nerdtree then close
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Disable menu bars
:set guioptions-=m  "remove menu bar
:set guioptions-=T  "remove toolbar
:set guioptions-=r  "remove right-hand scroll bar
:set guioptions-=L  "remove left-hand scroll bar
""  Moving Around/Editing
set cursorline              "  have a line indicate the cursor location
set background=dark
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
colorscheme nord

let g:nord_italic_comments = 1
let g:nord_comment_brightness = 12
let g:nord_uniform_diff_background = 1
let g:nord_cursor_line_number_background = 1

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

if has("mouse")
    set mouse=a
endif

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

set omnifunc=syntaxcomplete#Complete
" Use deoplete.
let g:deoplete#enable_at_startup = 1

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
nmap     <Leader>gs :Gstatus<CR>gg<c-n>
nnoremap <Leader>d :Gdiff<CR>
nnoremap <Leader>b :Gblame<CR>

"Remove all trailing whitespace by pressing F5
nnoremap <F5> :let _s=@/<Bar>:%s/\s\+$//e<Bar>:let @/=_s<Bar><CR>

" ----------------------------------------------------------------------------
" undotree
" ----------------------------------------------------------------------------
"let g:undotree_WindowLayout = 2
nnoremap <F9> :UndotreeToggle<CR>
nnoremap <F6> :YcmRestartServer<CR>

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
nnoremap <silent> <Leader>gb       :GoBuild<CR>
nnoremap <silent> <Leader>gr       :GoRun<CR>
nnoremap <silent> <Leader>gt       :GoTest<CR>

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

set statusline+=%#warningmsg#
set statusline+=%{SyntasticStatuslineFlag()}
set statusline+=%*

"Enable ident guide for default
au VimEnter * IndentGuidesEnable

let g:syntastic_always_populate_loc_list = 1
let g:syntastic_auto_loc_list = 1
let g:syntastic_check_on_open = 1
let g:syntastic_check_on_wq = 0

" Disable some irritating mappings
let g:sexp_enable_insert_mode_mappings = 0
"noremap <Up> <Nop>
"noremap <Down> <Nop>
"noremap <Left> <Nop>
"noremap <Right> <Nop>

"go config
let g:go_fmt_command = "goimports"

"ultisnips
"let g:UltiSnipsExpandTrigger="<c-j>"
"let g:UltiSnipsJumpForwardTrigger="<c-j>"
"let g:UltiSnipsJumpBackwardTrigger="<c-k>"

function! g:UltiSnips_Complete()
  call UltiSnips#ExpandSnippet()
  if g:ulti_expand_res == 0
    if pumvisible()
      return "\<C-n>"
    else
      call UltiSnips#JumpForwards()
      if g:ulti_jump_forwards_res == 0
        return "\<TAB>"
      endif
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


if !exists("g:UltiSnipsJumpForwardTrigger")
  let g:UltiSnipsJumpForwardTrigger = "<tab>"
endif

if !exists("g:UltiSnipsJumpBackwardTrigger")
  let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
endif

au InsertEnter * exec "inoremap <silent> " . g:UltiSnipsExpandTrigger     . " <C-R>=g:UltiSnips_Complete()<cr>"
au InsertEnter * exec "inoremap <silent> " .     g:UltiSnipsJumpBackwardTrigger . " <C-R>=g:UltiSnips_Reverse()<cr>"


" vim terraform
let g:terraform_align=1

