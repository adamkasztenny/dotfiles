call plug#begin('~/.vim/plugged')
set nocompatible              " :/be iMproved, required
filetype off                  " required
set encoding=utf8

" Make sure you use single quotes

" Multiple Plug commands can be written in a single line using | separators
Plug 'SirVer/ultisnips' | Plug 'honza/vim-snippets'

" Using a tagged release; wildcard allowed (requires git 1.9.2 or above)
Plug 'fatih/vim-go', { 'tag': '*' }

" Plugin outside ~/.vim/plugged with post-update hook
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
Plug 'junegunn/fzf.vim'

" Async lint
Plug 'w0rp/ale'

Plug 'itchyny/lightline.vim'

Plug 'terryma/vim-multiple-cursors'

Plug 'tpope/vim-eunuch'

Plug 'scrooloose/nerdtree'
Plug 'Xuyuanp/nerdtree-git-plugin'
Plug 'ryanoasis/vim-devicons'

" Git
Plug 'tpope/vim-fugitive'
Plug 'mhinz/vim-signify'
Plug 'junegunn/gv.vim'

Plug 'mhartington/oceanic-next'
Plug 'hashivim/vim-terraform'
Plug 'juliosueiras/vim-terraform-completion'

if has('nvim')
  Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
else
  Plug 'Shougo/deoplete.nvim'
  Plug 'roxma/nvim-yarp'
  Plug 'roxma/vim-hug-neovim-rpc'
endif

Plug 'zchee/deoplete-go', { 'do': 'make'}

" Track the engine.
Plug 'SirVer/ultisnips'

" Snippets are separated from the engine. Add this if you want them:
Plug 'honza/vim-snippets'

"ident line
Plug 'Yggdroot/indentLine'

Plug 'rizzatti/dash.vim'

Plug 'tpope/vim-surround'

Plug 'majutsushi/tagbar'


let g:python3_host_prog = '/Users/lucas/.pyenv/shims/python3.6'
call plug#end()

" Initialize plugin system
" terraform
let g:terraform_align=1
let g:terraform_commentstring='//%s'
let g:terraform_fmt_on_save=1

"deoplete
let g:deoplete#enable_at_startup = 1

if (&ft=='terraform' || &ft=='tf')
  let g:deoplete#omni_patterns = {}

  call deoplete#custom#option('omni_patterns', {
        \ 'complete_method': 'omnifunc',
        \ 'terraform': '[^ *\t"{=$]\w*',
        \})
endif

call deoplete#initialize()

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger="<tab>"
let g:UltiSnipsJumpForwardTrigger="<c-b>"
let g:UltiSnipsJumpBackwardTrigger="<c-z>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

" (Optional)Remove Info(Preview) window
set completeopt-=preview

" (Optional)Hide Info(Preview) window after completions
autocmd CursorMovedI * if pumvisible() == 0|pclose|endif
autocmd InsertLeave * if pumvisible() == 0|pclose|endif

" (Optional) Enable terraform plan to be include in filter
let g:syntastic_terraform_tffilter_plan = 1

" (Optional) Default: 0, enable(1)/disable(0) plugin's keymapping
let g:terraform_completion_keys = 1

" (Optional) Default: 1, enable(1)/disable(0) terraform module registry completion
let g:terraform_registry_module_completion = 0
" For Neovim 0.1.3 and 0.1.4
let $NVIM_TUI_ENABLE_TRUE_COLOR=1
let g:airline_theme='oceanicnext'

" Or if you have Neovim >= 0.1.5
if (has("termguicolors"))
 set termguicolors
endif

" Theme
syntax enable
colorscheme OceanicNext

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

set noerrorbells
set confirm                 " Y-N-C prompt if closing with unsaved changes.
set showcmd                 " Show incomplete normal mode commands as I type.
set report=0                " : commands always print changed line count.
set shortmess+=a            " Use [+]/[RO]/[w] for modified/readonly/written.
set ruler                   " Show some info, even without statuslines.
set laststatus=2            " Always show statusline, even if only 1 window.
set statusline=%<[%n]\ %F\ %m%r%y\ %{exists('g:loaded_fugitive')?fugitive#statusline():''}\ %=%-14.(%l,%c%V%)\ %P


""" Map Keys
nnoremap <leader>w :w!<CR>
nnoremap <leader>q :q!<CR>
nnoremap <leader>l <C-w>
nnoremap <leader>L <C-W>

"trim whitespaces
autocmd BufWritePre * %s/\s\+$//e

" <F10> | NERD Tree
nnoremap <F10> :NERDTreeToggle<cr>
nnoremap <leader>n :NERDTreeToggle<cr>

nmap <F8> :TagbarToggle<CR>


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

" Unselect the search result
map <Leader><Space> :noh<CR>

" ============================================================================
" FZF {{{
" ============================================================================

" nnoremap <silent> <Leader><Leader> :Files<CR>
" nnoremap <leader>s :FZF<CR>
nnoremap <silent> <expr> <Leader>s (expand('%') =~ 'NERD_tree' ? "\<c-w>\<c-w>" : '').":Files\<cr>"
nnoremap <silent> <Leader>C        :Colors<CR>
nnoremap <silent> <Leader><Enter>  :Buffers<CR>
nnoremap <silent> <Leader>ag       :Ag <C-R><C-W><CR>
nnoremap <silent> <Leader>AG       :Ag <C-R><C-A><CR>
nnoremap <silent> <Leader>`        :Marks<CR>

function! s:fzf_statusline()
  " Override statusline as you like
  highlight fzf1 ctermfg=161 ctermbg=251
  highlight fzf2 ctermfg=23 ctermbg=251
  highlight fzf3 ctermfg=237 ctermbg=251
  setlocal statusline=%#fzf1#\ >\ %#fzf2#fz%#fzf3#f
endfunction
autocmd! User FzfStatusLine call <SID>fzf_statusline()

" ============================================================================
" Go Configuration {{{
" ============================================================================
let g:go_fmt_command = "goimports"
let g:tagbar_type_go = {
	\ 'ctagstype' : 'go',
	\ 'kinds'     : [
		\ 'p:package',
		\ 'i:imports:1',
		\ 'c:constants',
		\ 'v:variables',
		\ 't:types',
		\ 'n:interfaces',
		\ 'w:fields',
		\ 'e:embedded',
		\ 'm:methods',
		\ 'r:constructor',
		\ 'f:functions'
	\ ],
	\ 'sro' : '.',
	\ 'kind2scope' : {
		\ 't' : 'ctype',
		\ 'n' : 'ntype'
	\ },
	\ 'scope2kind' : {
		\ 'ctype' : 't',
		\ 'ntype' : 'n'
	\ },
	\ 'ctagsbin'  : 'gotags',
	\ 'ctagsargs' : '-sort -silent'
  \ }


nnoremap <silent> <Leader>gb       :GoBuild<CR>
nnoremap <silent> <Leader>gr       :GoRun<CR>
nnoremap <silent> <Leader>gt       :GoTest<CR>
nnoremap <silent><Leader>t         :GoDecls<CR>
nnoremap <silent><Leader>gdf       :GoDefs<CR>

let g:go_term_mode = "split"
let g:go_term_height = 60
let g:go_term_width = 60

" ============================================================================
" Disable Arrows {{{
" ============================================================================
noremap <Up> <NOP>
noremap <Down> <NOP>
noremap <Left> <NOP>
noremap <Right> <NOP>

