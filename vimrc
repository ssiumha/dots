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

set nowrap
set list listchars=tab:\|\ ,trail:-,nbsp:+,extends:>,precedes:<

set timeoutlen=250

set iskeyword+=\-,$

set completeopt=menu,menuone,longest
set wildignorecase
"set wildoptions+=fuzzy

set cursorcolumn

"set spell spellfile=$HOME/dots/vim/spell/en.utf-8.add
" highlight SpellBad cterm=underline ctermbg=88 gui=underline guibg=#902020 guisp=NvimLightRed

" terminal

if has('nvim')
  set scrollback=50000
  set laststatus=3 " global status line
endif

"----------------
" variables
"----------------
" netrw
let g:netrw_fastbrowse = 2 " prevent reset cursor. but want refresh, need <c-l>
let g:netrw_keepdir = 0 " auto cd
"let g:netrw_liststyle = 3 " tree mode

"----------------
" fold
"----------------

" highlight Folded ctermfg=145 ctermbg=236 gui=italic guifg=#a0a8b0 guibg=#384048
autocmd ColorScheme * highlight Folded cterm=NONE ctermbg=236 ctermfg=145 gui=italic guibg=NONE guifg=#a0a8b0

" highlight FoldColumn cterm=NONE ctermbg=NONE guibg=NONE

set fillchars=fold:\ ,vert:\
" set foldcolumn=2
" set foldtext=MyFoldText()
" func! MyFoldText()
"   " v:foldlevel
"   return getline(v:foldstart) . ' ... ' . (v:foldend - v:foldstart + 1) . ' lines'
" endfunc

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
" let g:ruby_host_prog = '$HOME/.local/bin/neovim-ruby-host'

set runtimepath^=~/dots/vim

"----------------
" complete
"----------------
set dictionary+=$HOME/.firm/dict.txt
iab destory destroy
iab functino function
iab lien line
iab exmaple example

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
  let $FZF_DEFAULT_COMMAND="fd -tf --hidden --no-ignore-vcs --follow --exclude 'tmp/' --exclude 'dist/' --exclude '.bundle/'"
  let g:fzf_preview_window = ['down,50%,<70(down,40%)', 'ctrl-/']
  if exists('$TMUX')
    let g:fzf_layout = { 'tmux': '90%,70%' }
  endif

  nnoremap <space>p <esc>:Files<cr>
  nnoremap <space>p[ <esc>:History<cr>
  nnoremap <space>pb <esc>:Buffers<cr>
  nnoremap <space>pt <esc>:Tags<cr>
  " nnoremap <space>ps <esc>:Snippets<cr> " TODO: symbol
  nnoremap <space>pc <esc>:Commands<cr>
  nnoremap <space>pj <esc>:Jumps<cr>
  nnoremap <space>pl <esc>:Lines<cr>
  nnoremap <space>pL <esc>:BLines<cr>
  nnoremap <space>pm <esc>:Marks<cr>
  nnoremap <space>pr <esc>:Rg<space>

  nmap <leader><tab> <plug>(fzf-maps-n)

  imap <c-x><c-k> <plug>(fzf-complete-word)
  imap <c-x><c-f> <plug>(fzf-complete-path)
  imap <c-x><c-l> <plug>(fzf-complete-line)

  " TODO
  " func! FzfAction(list, action) abort

Plug 'voldikss/vim-floaterm'
  let g:floaterm_width = 0.95
  let g:floaterm_height = 0.8

  " FloatermCmd('ls', { out -> setline('.', out) })
  " FloatermCmd('fzf', { out -> append(line('.') - 1, out) })
  " FloatermCmd('fzf', { path -> execute('read ' . path) }, 'filepath')
  " FloatermCmd(printf('$HOME/dots/bin/snip %s %s', &ft, expand('%:p')), { path -> execute('read ' . path) }, 'filepath')
  func! FloatermCmd(cmd, action, return_type='stdout') abort
    try
      let l:tempfile = tempname()
      let [shell, shellslash, shellcmdflag, shellxquote] = floaterm#util#use_sh_or_cmd()
      let newcmd = [&shell, &shellcmdflag, a:cmd . ' > ' . l:tempfile]
      " @param job [number] ex) 3
      " @param data [number] ex) 0
      " @param event [string] ex) 'exit'
      " @param opener [string] ex) 'split'
      let jobopts = { 'on_exit': funcref({ job, data, event, opener -> a:action(a:return_type == 'filepath' ? l:tempfile : readfile(l:tempfile)) }) }
      let config = {}
      let bufnr = floaterm#terminal#open(-1, newcmd, jobopts, config)
    finally
      let [&shell, &shellslash, &shellcmdflag, &shellxquote] = [shell, shellslash, shellcmdflag, shellxquote]
    endtry
  endfunc

  command! -nargs=* -complete=customlist,floaterm#cmdline#complete -bang -range MySnip
        \ call FloatermCmd(printf('$HOME/dots/bin/snip %s %s', &ft, expand('%:p')), { path -> execute('read ' . path) }, 'filepath')
  nnoremap <space>f <esc>:MySnip<cr>

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
        \           [] ],
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
Plug 'kana/vim-metarw' " TODO: webdav

Plug 'tpope/vim-dadbod'
Plug 'kristijanhusak/vim-dadbod-ui'
" Plug 'kristijanhusak/vim-dadbod-completion' "Optional

Plug 'powerman/vim-plugin-AnsiEsc'
Plug 'tpope/vim-commentary'
Plug 'tpope/vim-repeat'
Plug 'tpope/vim-surround'
Plug 'michaeljsmith/vim-indent-object'

" ggandor/leap.nvim?
Plug 'justinmk/vim-sneak'
  let g:sneak#s_next = 1

  nmap <m-c-s> <Plug>Sneak_S
  nmap <c-s> <Plug>Sneak_s

Plug 'jpalardy/vim-slime'
  let g:slime_target = 'tmux'
  let g:slime_paste_file = tempname()
  " let g:slime_no_mappings = 1
  func! s:slime_wrapper()
    " TODO C-c wrapper
    " let b:slime_config["socket_name"] = "default"
    " let b:slime_config["target_pane"] = "%1" -> complete fzf
  endfunc

Plug 'junegunn/vim-easy-align'
  xmap <space>ga <Plug>(LiveEasyAlign)
  nmap <space>ga <Plug>(LiveEasyAlign)

"Plug 'rhysd/vim-lsp-ale'
Plug 'dense-analysis/ale'
  let g:ale_linters = {}
  let g:ale_fixers = {}

  let g:ale_enabled = 1
  let g:ale_completion_autoimport = 1
  let g:ale_disable_lsp = 1
  let g:ale_fix_on_save = 1
  let g:ale_lint_on_save = 1
  let g:ale_linters_explicit = 1

  let g:ale_virtualtext = 1
  let g:ale_virtualtext_cursor = 1
  let g:ale_virtualtext_prefix = '>>'

  let g:ale_set_loclist = 0
  let g:ale_set_quickfix = 0
  let g:ale_use_neovim_diagnostics_api = 1
  " let g:ale_disable_lsp = 'auto'
  " let g:ale_completion_enabled = 1
  " set omnifunc=ale#completion#OmniFunc

" Lang
Plug 'tpope/vim-rails', { 'for': 'ruby' }
Plug 'hashivim/vim-terraform', { 'for': 'terraform' }
Plug 'elixir-editors/vim-elixir', { 'for': 'elixir' }
Plug 'NoahTheDuke/vim-just', { 'for': 'just' }

Plug 'github/copilot.vim'
  let g:copilot_filetypes = {
        \ '*': v:true,
        \ }
  " let g:copilot_no_tab_map = v:true
  " imap <silent><script><expr> <tab> copilot#Accept("\<CR>")

call plug#end()

"----------------
" plug:after
"----------------
colorscheme jellybeans

highlight TreesitterContext guibg=gray ctermbg=8

highlight IndentLevel1 guibg=#ff0000 ctermbg=red
highlight IndentLevel2 guibg=#ffff00 ctermbg=yellow
highlight IndentLevel3 guibg=#ffa500 ctermbg=blue
highlight IndentLevel4 guibg=#00ff00 ctermbg=green
" syntax match IndentLevel1 /^ \{2}/ containedin=ALL
" syntax match IndentLevel2 /^ \{2}\zs \{2}\ze/ containedin=ALL
" syntax match IndentLevel3 /^ \{4}\zs \{2}\ze/ containedin=ALL
" syntax match IndentLevel4 /^ \{6} \{2}/ containedin=ALL contained
" call matchadd('IndentLevel2', "^ ")

autocmd FileType yaml
      \ setlocal nofoldenable foldmethod=expr foldexpr=nvim_treesitter#foldexpr()

if has('nvim-0.7.0')
  lua require 'vimrc'
endif

"----------------
" plug:after ale
"----------------
let g:ale_linters['typescript'] = ['biome']
let g:ale_fixers['typescript'] = ['my_biome']

let g:ale_linters['typescriptreact'] = ['biome']
let g:ale_fixers['typescriptreact'] = ['my_biome']

" let g:ale_biome_options = '--no-ignore'
" let g:ale_biome_fixer_apply_unsafe = 1
func! MyAleFixBiome(buffer) abort
    let l:executable = ale#handlers#biome#GetExecutable(a:buffer)
    let l:cmd = printf('%s check --apply %%t', ale#Escape(l:executable))
    return {  'command': l:cmd, 'read_temporary_file': 1 }
endfunc
call ale#fix#registry#Add('my_biome', 'MyAleFixBiome', ['typescript'], 'my custom biome')

let g:ale_linters['sh'] = ['shellcheck']
let g:ale_linters['bash'] = ['shellcheck']

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

command! EchoSyntax
      \ for id in synstack(line('.'), col('.'))
      \| echomsg synIDattr(id, 'name')
      \| endfor

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

  autocmd FileType qf nnoremap <buffer> q :cclose<cr>
augroup END

"----------------
" variant
"----------------
if has('ide') "ideavimrc
  set noexpandtab

  inoremap <c-p> <c-o>:action CodeCompletion<cr>
  inoremap <c-n> <c-o>:action CodeCompletion<cr>

  nnoremap <space>r :action RunClass<cr>
endif

if exists('g:neovide')
  let g:neovide_transparency = 0.9
  nnoremap <D-v> "+p
  inoremap <D-v> <c-r>+
  tnoremap <D-v> <c-r>+

  autocmd VimEnter * if argc() == 0
        \| exe 'cd ' . g:gtd#dir | exe 'GtdReview'
        \| endif
endif

if has('gui_macvim')
  set guifont=Menlo:h14
  set nospell nowrap concealcursor=

  autocmd VimEnter * if argc() == 0
        \| exe 'cd ~/docs' | edit index.md
        \| endif

  " :h macvim-prefs
  "
  "   MMLoginShellArgument =
  "   defaults read org.vim.MacVim
  "   defaults write org.vim.MacVim MMTranslateCtrlClick 0
  "   defaults delete org.vim.MacVim
  "
  " let &shellcmdflag=-l\ -c
  " not working MISE_CACHE_PATH
endif
