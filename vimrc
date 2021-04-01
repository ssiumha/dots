" vim: ff=unix foldmethod=marker
scripte utf-8
source $HOME/dotfiles/vimrc.minimal

" env
let s:local_vim_path = expand('$HOME/.local/vim')
let s:dot_vim_path = expand('$HOME/dotfiles/vim')

let s:is_ms_windows = has('win32') || has('win16')

let mapleader = '\\'
let maplocalleader = '\\'

execute 'set runtimepath^='.s:dot_vim_path

"set: editing {{{
set tabstop=2 shiftwidth=0 softtabstop=-1
set expandtab shiftround
set backspace=2 "indent,eol,start

set nowrap

set timeoutlen=200  "delay of an input key sequence
"}}}

"set: layout {{{
set laststatus=2

set background=dark
colorscheme horizon
"}}}

"set: file {{{
let g:netrw_home=expand('$HOME/.vim')

set undofile undoreload=1000
set backup swapfile
let &undodir = expand(s:local_vim_path . '/tmp/undo')
let &backupdir = expand(s:local_vim_path . '/tmp/backup')
let &directory = expand(s:local_vim_path . '/tmp/swap')

set wildignore+=.hg,.git,.svn
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest
set wildignore+=*.DS_Store
set wildignore+=*.luac,*.pyc
set wildignore+=*.meta
"}}}

"set: gui {{{
if s:is_ms_windows
    set mouse=
    set guifont=DejaVu_Sans_Mono:h11:w6:cANSI "download: http://dejavu-fonts.org/wiki/Download
    set guifontwide=Dotumche:h10.5:cCHINESEBIG5
endif

if has('gui_running')
    set guioptions=
endif
"}}}

"keymap {{{
nnoremap <silent> *
            \ :let stay_view = winsaveview()<cr>*:call winrestview(stay_view)<cr>

vnoremap * :<c-u>call <sid>visualSearch()<cr>//<cr><c-o>
vnoremap # :<c-u>call <sid>visualSearch()<cr>??<cr><c-o>
func! s:visualSearch()
    let temp = @@
    norm! gvy
    let @/ = '\V' . substitute(escape(@@, '/\'), '\n', '\\n', 'g')
    let @@ = temp
endfunc

nnoremap <space>cd :cd %:p:h<cr>
nnoremap <space>lc :lc %:p:h<cr>

nnoremap <space>n <esc>:Lexplore \| vertical resize 24<cr>
nnoremap <space>nc <esc>:Lexplore %:p:h \| vertical resize 24<cr>
"}}}

"plug {{{
let s:vim_plug_install_path = expand('$HOME/.vim/autoload/plug.vim')

func! InstallVimPlug()
    call system("curl -fLo ".s:vim_plug_install_path." --create-dirs
            \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim")
endfunc

if filereadable(s:vim_plug_install_path)
    call plug#begin(expand(s:local_vim_path . '/plugged'))
        " External
        Plug 'mileszs/ack.vim'
        Plug 'simnalamburt/vim-tiny-ime', { 'do' : './build' }

        Plug 'tpope/vim-fugitive'
        Plug 'neoclide/coc.nvim', {'branch': 'release'}

        Plug 'junegunn/fzf' ", { 'do': { -> fzf#install() } }
        Plug 'junegunn/fzf.vim'

        Plug 'diepm/vim-rest-console'

        " UI
        Plug 'itchyny/lightline.vim'
        Plug 'majutsushi/tagbar'
        Plug 'nathanaelkane/vim-indent-guides'

        " Edit
        Plug 'tpope/vim-surround'
        Plug 'junegunn/vim-easy-align'
        Plug 'mechatroner/rainbow_csv'
        Plug 'ctrlpvim/ctrlp.vim'

        " Language
        Plug 'powerman/vim-plugin-AnsiEsc'
        Plug 'digitaltoad/vim-pug'

        Plug 'pangloss/vim-javascript'
        Plug 'mxw/vim-jsx'

        Plug 'leafgarland/typescript-vim'
        Plug 'peitalin/vim-jsx-typescript'
    call plug#end()


    augroup filetype_plug
        autocmd BufNewFile,BufRead *.tsx    setfiletype typescript.tsx
    augroup END
else
    echo 'not installed vim-plug, please :call InstallVimPlug()'
endif
"}}}

"plug: ack {{{
nnoremap <space>a :Ack!<space>
if executable('rg')
  let g:ackprg = 'rg --vimgrep --smart-case --color=never'
elseif executable('ag')
  let g:ackprg = 'ag --vimgrep --smart-case'
endif
"}}}

"plug: ctrlp {{{
let g:ctrlp_map = ''
let g:ctrlp_cmd = 'CtrlPCurWD'
let g:ctrlp_lazy_update = 250 "ms
let g:ctrlp_clear_cache_on_exit = 0
let g:ctrlp_cache_dir = (s:is_ms_windows
            \   ? expand(s:local_vim_path . '/ctrlp_win')
            \   : expand(s:local_vim_path . '/ctrlp')
            \ )
let g:ctrlp_custom_ignore = {
            \ 'dir': '\v[\/](AppData[\/]Local[\/]Temp)|(\.(git|svn))$|node_module',
            \ 'file': '\v\.(pyc|svn-base|meta|prefab|class)$',
            \ }

if has('job') || has('nvim')
    let g:ctrlp_user_command_async = 1
endif

nnoremap <c-p> <esc>:CtrlP<cr>
"nnoremap <space>p <esc>:CtrlPCurWD<cr>
nnoremap <space>pp <esc>:CtrlPBuffer<cr>
nnoremap <space>p[ <esc>:CtrlPMRUFiles<cr>
nnoremap <space>pt <esc>:CtrlPTag<cr>
nnoremap <space>ptt <esc>:CtrlPBufTag<cr>
"}}}

"plug: fzf {{{
nnoremap <space>p <esc>:Files<cr>
"}}}

"plug: lightline {{{
let g:lightline = { 'colorscheme' : 'horizon' }
"}}}

"plug: tagbar {{{
nnoremap <silent> <space>t :TagbarToggle<cr>
"}}}

"plug: indent-guides {{{
let g:indent_guides_enable_on_vim_startup = 1
let g:indent_guides_indent_levels = 16
let g:indent_guides_guide_size = 1
let g:indent_guides_start_level = 2

let g:indent_guides_auto_colors = 0
hi IndentGuidesEven  ctermbg=black
"hi IndentGuidesOdd ctermbg=black
" }}}

"plug: vim-rest-console {{{
autocmd FileType rest :noremap <buffer> <cr> :call VrcQuery()<cr>

let g:vrc_response_default_content_type = 'application/json'
let g:vrc_allow_get_request_body = 1

let g:vrc_auto_format_response_patterns = {
  \ 'json': 'jq',
  \ 'xml': 'xmllint --format -',
\}

let g:vrc_curl_opts = {}
let g:vrc_curl_opts['--connect-timeout'] = 10
let g:vrc_curl_opts['--silent'] = ''
let g:vrc_curl_opts['-L'] = '' "redirect
let g:vrc_curl_opts['-i'] = '' "include header
"}}}

"plug: built-in {{{
let g:netrw_list_hide = '\.pyc$,\~$,\.meta$'

"" default plugin off
let did_install_default_menus = 0
let did_install_syntax_menu = 0
let do_syntax_sel_menu = 0
"}}}

"functions {{{
func! s:stripAnsiColorCode()
  execute "%!perl -MTerm::ANSIColor=colorstrip -ne 'print colorstrip $_'"
endfunc
command! AnsiStrip :call s:stripAnsiColorCode()

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
"}}}

"autocmd: * {{{
augroup filetype_all
    autocmd!

    autocmd BufWritePre * :call s:removeTrailingWhitespace()
    autocmd BufWritePre * :call s:ensureParentDirectory()
    autocmd BufReadPost * :call s:moveCursorToLastPosition()
augroup END
"}}}

"autocmd: markdown {{{
augroup github_markdown
    autocmd!

    " github markdown에서는 _에 기능이 없으므로 제거한다
    autocmd FileType markdown :syn clear markdownError markdownItalic

    " TODO markdownLinkText의 conceal 기능 추가
    autocmd FileType markdown :setlocal conceallevel=1 concealcursor=
augroup ENd
"}}}
