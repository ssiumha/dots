" vim: ff=unix foldmethod=marker
scripte utf-8
source $HOME/dotfiles/vimrc.minimal

" default plugin off
let did_install_default_menus = 0
let did_install_syntax_menu = 0
let do_syntax_sel_menu = 0

let s:local_vim_path = expand('~/.local/vim')

"set: editing {{{
set tabstop=4 shiftwidth=0 softtabstop=-1
set expandtab shiftround
set backspace=2 "indent,eol,start
"}}}

"set: layout {{{
set laststatus=2
"}}}

"set: file {{{
set undofile undoreload=1000
set backup swapfile
let &undodir = expand(s:local_vim_path . '/tmp/undo')
let &backupdir = expand(s:local_vim_path . '/tmp/backup')
let &directory = expand(s:local_vim_path . '/tmp/swap')
"}}}


"plug {{{
let s:vim_plug_install_path = expand('~/.vim/autoload/plug.vim')

func! InstallVimPlug()
    call system("curl -fLo ".s:vim_plug_install_path." --create-dirs
            \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim")
endfunc

if filereadable(s:vim_plug_install_path)
    call plug#begin(expand(s:local_vim_path . '/plugged'))
        Plug 'ctrlpvim/ctrlp.vim'
        Plug 'itchyny/lightline.vim'
        Plug 'mileszs/ack.vim'
    call plug#end()
else
    echo 'not installed vim-plug, please :call InstallVimPlug()'
endif
"}}}

"plug: ack {{{
nnoremap <space>a :Ack!<space>
if executable('ag')
    let g:ackarg = 'ag --vimgrep --smart-case'
endif
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
