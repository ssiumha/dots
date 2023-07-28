" vim: ff=unix
scriptencoding utf-8

syntax enable

" set
set hlsearch
set ignorecase smartcase incsearch wrapscan
set gdefault

set clipboard=unnamed,unnamedplus ",autoselect

" key mapping
cnoremap <c-a> <home>
cnoremap <c-e> <end>

inoremap <c-a> <home>
inoremap <c-e> <end>
inoremap <c-f> <c-o>w
inoremap <c-b> <c-o>b

inoremap <c-h> <left>
inoremap <c-j> <down>
inoremap <c-k> <up>
inoremap <c-l> <right>

noremap ; :
noremap : ;

noremap j gj
noremap k gk
noremap gj j
noremap gk k

nnoremap Y yg_

noremap <c-h> <c-w>h
noremap <c-j> <c-w>j
noremap <c-k> <c-w>k
noremap <c-l> <c-w>l

vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

nnoremap <esc><esc> :noh<cr>

" key mapping more
nnoremap <space>cd :cd %:p:h<cr>
nnoremap <space>lc :lc %:p:h<cr>

" edit
set tabstop=2 shiftwidth=0 softtabstop=-1
set expandtab shiftround
set backspace=2 "indent,eol,start

set timeoutlen=200

"----------------
" file
"----------------
set undofile undoreload=1000
set backup swapfile
let &undodir = expand('$HOME/.cache/vim/undo')
let &backupdir = expand('$HOME/.cache/vim/backup')
let &directory = expand('$HOME/.cache/vim/swap')

"----------------
" plug
"----------------
let s:vim_plug_install_path = expand('$HOME/.vim/autoload/plug.vim')
if !filereadable(s:vim_plug_install_path)
  call system("curl -fLo ".s:vim_plug_install_path." --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim")
endif

call plug#begin(expand('$HOME/.local/vim/plugged'))

Plug 'junegunn/fzf'
Plug 'junegunn/fzf.vim'
  let $FZF_DEFAULT_COMMAND="fd -tf --hidden"
  nnoremap <space>p <esc>:Files<cr>
  nnoremap <space>p[ <esc>:History<cr>
  nnoremap <space>pb <esc>:Buffers<cr>

Plug 'mileszs/ack.vim'
  nnoremap <space>a :Ack!<space>
  let g:ackprg = 'rg --vimgrep --smart-case --color=never'

Plug 'tpope/vim-fugitive', { 'on': ['Git'] }

" UI
Plug 'itchyny/lightline.vim'
Plug 'nanotech/jellybeans.vim'

" Utils
Plug 'powerman/vim-plugin-AnsiEsc'
Plug 'tpope/vim-commentary'

if has('python3') " ref: checkhealth provider
  Plug 'SirVer/UltiSnips'
    let g:UltiSnipesExpandTrigger="<tab>"
    let g:UltiSnipsJumpForwardTrigger="<tab>"
    let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
    let g:UltiSnipsEditSplit="vertical"
    let g:UltiSnipsSnippetDirectories=['snips'] " config/nvim/snips
endif

" Lang
Plug 'tpope/vim-rails'
Plug 'hashivim/vim-terraform'

if has('nvim-0.7.0')
  Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }
  Plug 'nvim-treesitter/nvim-treesitter-context'
  Plug 'nvim-treesitter/nvim-treesitter-textobjects'

  highlight TreesitterContext guibg=gray ctermbg=8
endif

call plug#end()

"----------------
" plug:after
"----------------
colorscheme jellybeans

if has('nvim-0.7.0')
lua << EOF
  require'nvim-treesitter.configs'.setup {
    ensure_installed = { 'ruby', 'yaml' },
    highlight = { enable = true, additional_vim_regex_highlighting = false },

    textobjects = {
      select = {
        enable = true,
        lookahead = true,
        keymaps = {
          ['ib'] = '@block.inner',
          ['ab'] = '@block.outer',
          ['if'] = '@function.inner',
          ['af'] = '@function.outer',
          ['ip'] = '@parameter.inner',
          ['ap'] = '@parameter.outer',
        },
        selection_modes = {
          ['@block.outer'] = 'V',
          ['@block.inner'] = 'V',
          ['@function.outer'] = 'V',
          ['@function.inner'] = 'V',
        },
      },
      move = {
        enable = true,
        set_jumps = true,
        goto_next_start = {
          [']m'] = '@function.outer',
        },
        goto_next_end = {
          [']M'] = '@function.outer',
        },
        goto_previous_start = {
          ['[m'] = '@function.outer',
        },
        goto_previous_end = {
          ['[M'] = '@function.outer',
        },
      },
    },
  }

  require'treesitter-context'.setup{
    enable = true,
    patterns = {
      default = { 'class', 'function', 'method' }
    }
  }
EOF
endif

"----------------
" commands
"----------------
func! s:stripAnsiColorCode()
  execute "%!perl -MTerm::ANSIColor=colorstrip -ne 'print colorstrip $_'"
endfunc
command! AnsiStrip :call s:stripAnsiColorCode()


"----------------
" methods
"----------------
func! s:removeTrailingWhitespace()
  let [l:old_search, l:stay_view] = [@/, winsaveview()]

  if &filetype != 'diff'
    silent! execute '%s;\s\+$;;e'
  endif

  call winrestview(l:stay_view)
  let @/ = l:old_search
endfunc
command! TraillingWhitespace :call s:removeTrailingWhitespace()

func! s:ensureParentDirectory()
  let l:dir = expand('<afile>:p:h')
  if !isdirectory(l:dir)
    call mkdir(l:dir, 'p')
  endif
endfunc

func! s:moveCursorToLastPosition()
  if line("'\"") > 0 && line("'\"") <= line('$')
    execute 'norm! g`"zvzz'
  endif
endfunc

augroup filetype_all
  autocmd!

  autocmd BufWritePre * :call s:removeTrailingWhitespace()
  autocmd BufWritePre * :call s:ensureParentDirectory()
  autocmd BufReadPost * :call s:moveCursorToLastPosition()
augroup END

"----------------
" ideavimrc
"----------------
if has('ide')
  set noexpandtab

  inoremap <c-p> <c-o>:action CodeCompletion<cr>
  inoremap <c-n> <c-o>:action CodeCompletion<cr>

  nnoremap <space>r :action RunClass<cr>
endif
