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

" TODO: UpdateRemotePlugin

" let g:ruby_host_prog = '$(ASDF_RUBY_VERSION=system gem environment gemdir)/bin/neovim-ruby-host'

"----------------
" complete
"----------------
set dictionary+=$HOME/dotfiles/config/nvim/words/dict.txt
set dictionary+=$HOME/.firm/dict.txt
iab destory destroy
iab functino function
iab lien line
iab exmaple example

autocmd FileType * execute 'setlocal dict+=$HOME/dotfiles/config/nvim/words/'.&filetype.'.txt'

"----------------
" plug
"----------------
let s:vim_plug_install_path = expand('$HOME/.vim/autoload/plug.vim')
if !filereadable(s:vim_plug_install_path)
  call system("curl -fLo ".s:vim_plug_install_path." --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim")
endif

call plug#begin(expand('$HOME/.local/vim/plugged'))

Plug 'junegunn/fzf', { 'do': { -> fzf#install() } }
Plug 'junegunn/fzf.vim'
  let $FZF_DEFAULT_COMMAND="fd -tf --no-ignore-vcs"
  nnoremap <space>p <esc>:Files<cr>
  nnoremap <space>p[ <esc>:History<cr>
  nnoremap <space>pb <esc>:Buffers<cr>
  nnoremap <space>pc <esc>:Commands<cr>
  nnoremap <space>pj <esc>:Jumps<cr>
  nnoremap <space>pl <esc>:Lines<cr>
  nnoremap <space>pL <esc>:BLines<cr>
  nnoremap <space>pm <esc>:Marks<cr>
  nnoremap <space>ps <esc>:Snippets<cr>
  nnoremap <space>pr <esc>:Rg<space>

  nmap <leader><tab> <plug>(fzf-maps-n)

  imap <c-x><c-k> <plug>(fzf-complete-word)
  imap <c-x><c-f> <plug>(fzf-complete-path)
  imap <c-x><c-l> <plug>(fzf-complete-line)

Plug 'voldikss/vim-floaterm'
  command! -nargs=* -complete=customlist,floaterm#cmdline#complete -bang -range MySnip  call s:mysnip()
  nnoremap <space>f <esc>:MySnip<cr>

  func! s:mysnip() abort
    try
      let [shell, shellslash, shellcmdflag, shellxquote] = floaterm#util#use_sh_or_cmd()

      let s:mysnip_tempfile = tempname()
      let newcmd = [&shell, &shellcmdflag, '$HOME/dotfiles/bin/snip.sh ' . &filetype . ' "' . expand('%:p') . '" > ' . s:mysnip_tempfile]
      let jobopts = { 'on_exit': funcref('s:mysnip_on_exit') }
      let config = {}
      let bufnr = floaterm#terminal#open(-1, newcmd, jobopts, config)
    finally
      let [&shell, &shellslash, &shellcmdflag, &shellxquote] = [shell, shellslash, shellcmdflag, shellxquote]
    endtry
  endfunc

  func! s:mysnip_on_exit(job, data, event, opener) abort
    if filereadable(s:mysnip_tempfile)
      execute 'read ' . s:mysnip_tempfile
    endif
  endfunc

Plug 'mileszs/ack.vim'
  nnoremap <space>a :Ack!<space>
  let g:ackprg = 'rg --vimgrep --smart-case --color=never'

Plug 'tpope/vim-fugitive', { 'on': ['Git'] }

Plug 'vim-test/vim-test'

" UI
Plug 'itchyny/lightline.vim'
  let g:lightline = {
        \ 'component_function': {
        \   'filename': 'LightlineFilename',
        \ }
        \ }

  function! LightlineFilename()
    let root = fnamemodify(get(b:, 'git_dir'), ':h')
    let path = expand('%:p')
    if path[:len(root)-1] ==# root
      return path[len(root)+1:]
    endif
    return expand('%')
  endfunction


Plug 'nanotech/jellybeans.vim'

" Utils
Plug 'powerman/vim-plugin-AnsiEsc'
Plug 'tpope/vim-commentary'

Plug 'nathanaelkane/vim-indent-guides'
  let g:indent_guides_enable_on_vim_startup = 1
  let g:indent_guides_indent_levels = 16
  let g:indent_guides_guide_size = 2
  let g:indent_guides_start_level = 2
  let g:indent_guides_auto_colors = 0
  "highlight IndentGuidesEven ctermbg=darkgray

" TODO
" https://github.com/hrsh7th/nvim-cmp

" TODO reference syntax only
if has('python3') " ref: checkhealth provider
  Plug 'SirVer/UltiSnips'
    let g:UltiSnipesExpandTrigger="<tab>"
    let g:UltiSnipsJumpForwardTrigger="<tab>"
    let g:UltiSnipsJumpBackwardTrigger="<s-tab>"
    let g:UltiSnipsEditSplit="vertical"
    let g:UltiSnipsSnippetDirectories=['snips'] " config/nvim/snips

    autocmd FileType snippets setlocal expandtab
endif

Plug 'dense-analysis/ale'
  " let g:ale_fixers = {
  " \   '*': ['remove_trailing_lines', 'trim_whitespace'],
  " \   'yaml': ['yamllint'],
  " \}
  let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
  let g:ale_sign_error = '✘'
  let g:ale_sign_warning = '⚠'
  let g:ale_lint_on_text_changed = 'never'

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

if has('nvim-0.7.0') && has('lua')
lua << EOF
  require'nvim-treesitter.configs'.setup {
    ensure_installed = { 'ruby', 'yaml' },
    auto_install = true,
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

  autocmd StdinReadPost * :AnsiEsc
  autocmd StdinReadPost * setlocal nowrap buftype=nofile
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
