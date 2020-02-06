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
set tabstop=4 shiftwidth=0 softtabstop=-1
set expandtab shiftround
set backspace=2 "indent,eol,start
"}}}

"set: layout {{{
set laststatus=2

set background=dark
colorscheme horizon
"}}}

"set: file {{{
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
set mouse=
"}}}

"keymap {{{
nnoremap <silent> *
            \ :let stay_view = winsaveview()<cr>*:call winrestview(stay_view)<cr>
"}}}

"plug {{{
let s:vim_plug_install_path = expand('$HOME/.vim/autoload/plug.vim')

func! InstallVimPlug()
    call system("curl -fLo ".s:vim_plug_install_path." --create-dirs
            \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim")
endfunc

if filereadable(s:vim_plug_install_path)
    call plug#begin(expand(s:local_vim_path . '/plugged'))
        Plug 'ctrlpvim/ctrlp.vim'
        Plug 'itchyny/lightline.vim'
        Plug 'mileszs/ack.vim'
        Plug 'majutsushi/tagbar'

        Plug 'neoclide/coc.nvim', {'branch': 'release'}

        Plug 'digitaltoad/vim-pug'

        Plug 'pangloss/vim-javascript'
        Plug 'mxw/vim-jsx'

        Plug 'tpope/vim-fugitive'
    call plug#end()
else
    echo 'not installed vim-plug, please :call InstallVimPlug()'
endif
"}}}

"plug: ack {{{
nnoremap <space>a :Ack!<space>
if executable('ag')
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

if executable('ag')
    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
endif

nnoremap <c-p> <esc>:CtrlP<cr>
nnoremap <space>p <esc>:CtrlP<cr>
nnoremap <space>pp <esc>:CtrlPBuffer<cr>
nnoremap <space>p[ <esc>:CtrlPMRUFiles<cr>
nnoremap <space>pt <esc>:CtrlPTag<cr>
nnoremap <space>ptt <esc>:CtrlPBufTag<cr>
"}}}

"plug: lightline {{{
let g:lightline = { 'colorscheme' : 'horizon' }
"}}}

"plug: tagbar {{{
nnoremap <silent> <space>t :TagbarToggle<cr>
"}}}

"plug: built-in {{{
let g:netrw_list_hide = '\.pyc$,\~$,\.meta$'

"" default plugin off
let did_install_default_menus = 0
let did_install_syntax_menu = 0
let do_syntax_sel_menu = 0
"}}}

"functions {{{
func! s:removeTrailingWhiteSpace()
    let [l:old_search, l:stay_view] = [@/, winsaveview()]

    if &filetype != 'diff'
        silent! execute '%s;\s\+$;;e'
    endif

    call winrestview(l:stay_view)
    let @/ = l:old_search
endfunc

func! s:moveCursorToLastPosition()
    if line("'\"") > 0 && line("'\"") <= line('$')
        execute 'norm! g`"zvzz'
    endif
endfunc
"}}}

"autocmd {{{
augroup filetype_all
    autocmd!

    autocmd BufWritePre * :call s:removeTrailingWhiteSpace()
    autocmd BufReadPost * :call s:moveCursorToLastPosition()
augroup END
"}}}
