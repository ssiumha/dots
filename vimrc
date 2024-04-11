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

set timeoutlen=250

set iskeyword+=\-,$

set completeopt=menu,menuone,longest
"set wildoptions+=fuzzy

" terminal
set scrollback=50000

" netrw
let g:netrw_fastbrowse = 2 " prevent reset cursor. but want refresh, need <c-l>
"let g:netrw_liststyle = 3 " tree mode

"----------------
" file
"----------------
set undofile undoreload=1000
set backup swapfile
let &undodir = expand('$HOME/.cache/vim/undo')
let &backupdir = expand('$HOME/.cache/vim/backup')
let &directory = expand('$HOME/.cache/vim/swap')

" TODO: UpdateRemotePlugin
" TODO: gem install neovim --bindir ~/.local/bin
let g:ruby_host_prog = '$HOME/.local/bin/neovim-ruby-host'

set runtimepath^=~/dotfiles/vim

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
  let $FZF_DEFAULT_COMMAND="fd -tf --no-ignore-vcs --follow"
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
      let newcmd = [&shell, &shellcmdflag, '$HOME/dotfiles/bin/snip ' . &filetype . ' "' . expand('%:p') . '" > ' . s:mysnip_tempfile]
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
" TODO: https://github.com/dense-analysis/ale?tab=readme-ov-file#how-can-i-customise-the-statusline
Plug 'itchyny/lightline.vim'
  let g:lightline = {}
  let g:lightline.active = {
        \ 'left': [ [ 'mode', 'paste' ],
        \           [ 'readonly', 'filename', 'modified' ],
        \           [ 'treesitter' ] ],
        \ 'right': [ [ 'lineinfo' ],
        \            [ 'percent' ],
        \            [ 'fileformat', 'fileencoding', 'filetype' ] ] }
  let g:lightline.inactive = {
        \ 'left': [ [ 'filename' ] ],
        \ 'right': [ [ 'lineinfo' ],
        \            [ 'percent' ] ] }
  let g:lightline.tabline = {
        \ 'left': [ [ 'tabs' ] ],
        \ 'right': [ [ 'close' ] ] }
  let g:lightline.component_function = {
        \   'filename': 'LightlineFilename',
        \   'treesitter': 'LightlineTreesitter',
        \ }

  function! LightlineFilename()
    let root = fnamemodify(get(b:, 'git_dir'), ':h')
    let path = expand('%:p')
    if path[:len(root)-1] ==# root
      return path[len(root)+1:]
    endif
    return expand('%')
  endfunction

  " TODO yaml
  " InspectTree
  " echo nvim_treesitter#statusline({ "type_patterns": ['flow_node'] })
  function! LightlineTreesitter()
    try
      return nvim_treesitter#statusline(100)
    catch
      return ""
    endtry
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

Plug 'dense-analysis/ale'
  let g:ale_fixers = {
  \   'yaml': ['yamllint'],
  \   'javascript': ['eslint'],
  \   'typescript': ['eslint'],
  \}
  " \   '*': ['remove_trailing_lines', 'trim_whitespace'],

  let g:ale_linters_ignore = {'typescript': ['deno']}
  let g:ale_echo_msg_format = '[%linter%] %s [%severity%]'
  let g:ale_sign_error = '✘'
  let g:ale_sign_warning = '⚠'
  let g:ale_lint_on_text_changed = 'never'

  let g:ale_completion_enabled = 1
  set omnifunc=ale#completion#OmniFunc

  " getline(1, '$')
  " TODO
  function! FzfVimOmniComplete(...)
    return fzf#vim#complete(s:extend({
          \ 'prefix': '^.*$',
          \ 'source': ale#completion#OmniFunc(1, '')}, get(a:000, 0, fzf#wrap())))
  endfunction

Plug 'neovim/nvim-lspconfig'

Plug 'hrsh7th/nvim-cmp'
Plug 'hrsh7th/cmp-nvim-lsp'
Plug 'hrsh7th/cmp-buffer'
" Plug 'hrsh7th/cmp-path'
" Plug 'hrsh7th/cmp-cmdline'

Plug 'dcampos/cmp-snippy'
Plug 'dcampos/nvim-snippy'
  imap <expr> <Tab> snippy#can_expand_or_advance() ? '<Plug>(snippy-expand-or-advance)' : '<Tab>'
  imap <expr> <S-Tab> snippy#can_jump(-1) ? '<Plug>(snippy-previous)' : '<S-Tab>'
  smap <expr> <Tab> snippy#can_jump(1) ? '<Plug>(snippy-next)' : '<Tab>'
  smap <expr> <S-Tab> snippy#can_jump(-1) ? '<Plug>(snippy-previous)' : '<S-Tab>'
  xmap <Tab> <Plug>(snippy-cut-text)

" Lang
Plug 'tpope/vim-rails'
Plug 'hashivim/vim-terraform'

if has('nvim-0.7.0')
  Plug 'nvim-treesitter/nvim-treesitter', { 'do': ':TSUpdate' }
  Plug 'nvim-treesitter/nvim-treesitter-context'
  Plug 'nvim-treesitter/nvim-treesitter-textobjects'
endif

call plug#end()

"----------------
" plug:after
"----------------
colorscheme jellybeans

highlight TreesitterContext guibg=gray ctermbg=8

autocmd FileType yaml
      \ setlocal nofoldenable foldmethod=expr foldexpr=nvim_treesitter#foldexpr()

set runtimepath^=~/.cache/treesitter

if has('nvim-0.7.0')
  "autocmd Filetype ruby setlocal omnifunc=v:lua.vim.lsp.omnifunc
  augroup user_lsp_config
    autocmd!

    autocmd LspAttach * nnoremap <buffer> <silent> gd    <cmd>lua vim.lsp.buf.declaration()<CR>
    autocmd LspAttach * nnoremap <buffer> <silent> gi    <cmd>lua vim.lsp.buf.implementation()<CR>
    autocmd LspAttach * nnoremap <buffer> <silent> <c-]> <cmd>lua vim.lsp.buf.definition()<CR>
    autocmd LspAttach * nnoremap <buffer> <silent> K     <cmd>lua vim.lsp.buf.hover()<CR>
  augroup END

  luafile $HOME/dotfiles/vimrc.lua
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
