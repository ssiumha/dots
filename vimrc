" vim: ff=unix
scripte utf-8
let $LANG = 'en_US.utf-8'
set langmenu=en_US.UTF-8
set encoding=utf-8 fileencoding=utf-8
set fileencodings=utf-8,cp949,cp932,euc-jp,shift-jis,big5,ucs-2le,latin1

let did_install_default_menus = 0
let did_install_syntax_menu = 0
let do_syntax_sel_menu = 0


let s:vimrc_path = expand('~/dotfiles/vimrc')
let s:dot_vim_path = expand('~/dotfiles/vim')
let s:dot_vim_after_path = expand('~/dotfiles/vim/after')

" 각 폴더는 bootstrap.sh에서 보장시킨다
let s:local_path = expand('~/.local')
let s:local_vim_path = expand(s:local_path.'/vim')
let s:undodir_path = expand(s:local_vim_path.'/tmp/undo')
let s:backupdir_path = expand(s:local_vim_path.'/tmp/backup')
let s:swapdir_path = expand(s:local_vim_path.'/tmp/swap')

let s:is_company = $COMPANY_NAME != ''
let s:is_ms_windows = has('win32') || has('win16')
let s:is_gui_running = has('gui_running')
let s:is_gui_macvim = has('gui_macvim')
let s:is_term_macvim = !s:is_gui_macvim && has('mac')

"initalize {{{
if has('vim_starting')
    let &runtimepath = s:dot_vim_path.','.&runtimepath
    let &runtimepath = s:dot_vim_after_path.','.&runtimepath

    if s:is_ms_windows
        winsize 170 40

        "colorscheme slate
        "colorscheme desert "mac-term
        "colorscheme solarized "mac-term
        "
        set guifont=DejaVu_Sans_Mono:h11:w6:cANSI "download: http://dejavu-fonts.org/wiki/Download
        set guifontwide=Dotumche:h10.5:cCHINESEBIG5

        if v:version >= 800
            "set renderoptions=type:directx
        endif

        "let &shell=expand('D:\\msys64\\usr\\bin\\bash.exe')
        "set shellcmdflag=-c
        "set shellslash
        set noshelltemp
    endif

    if s:is_gui_running
        set guioptions=
        "full screen start
        "au GUIEnter * simalt ~x
    endif
endif
"}}}

filetype plugin on
syntax enable
set background=dark
colorscheme horizon
"}}}

"setup plugin {{{
if s:is_ms_windows
    let Tlist_Ctags_Cmd='' "take action ctags not found
endif

let g:ctrlp_cmd = 'CtrlPCurWD'
let g:ctrlp_clear_cache_on_exit = 0

if s:is_ms_windows
    let g:ctrlp_cache_dir = expand(s:local_vim_path.'/ctrlp_win')
else
    let g:ctrlp_cache_dir = expand(s:local_vim_path.'/ctrlp')
endif
let g:ctrlp_custom_ignore = {
    \ 'dir': '\v[\/](AppData[\/]Local[\/]Temp)|(\.(git|svn))$',
    \ 'file': '\v\.(pyc|svn-base|meta|prefab|class)$',
    \}


let g:netrw_list_hide = '\.pyc$,\~$,\.meta$'

let g:validator_auto_open_quickfix = 0
let g:validator_python_checkers = ['flake8']
let g:validator_javascript_checkers = ['eslint']
let g:validator_jsonlint_checkers = ['jsonlint']

let twitvim_enable_python = 1

let g:EasyMotion_keys = 'ajskdlf;ghqweruiop'
nmap , <Plug>(easymotion-s2)
xmap , <Plug>(easymotion-s2)
omap , <Plug>(easymotion-s2)

nmap g/ <Plug>(easymotion-sn)
xmap g/ <Plug>(easymotion-sn)
omap g/ <Plug>(easymotion-sn)

let g:multi_cursor_use_default_mapping=0
let g:multi_cursor_quit_key='<esc>'
nnoremap <silent> <space>m :MultipleCursorsFind <c-r>/<cr>
vnoremap <silent> <space>m :MultipleCursorsFind <c-r>/<cr>

let g:surround_no_mappings = 1
nnoremap cs <Plug>Csurround

let g:lightline = { 'colorscheme' : 'horizon' }
"}}}

"autocmd{{{
augroup filetype_all
    autocmd!

    " remove trailing whitespace
    autocmd BufWritePre *
                \  let [s:old_search, s:stay_view] = [@/, winsaveview()]
                \| silent! execute '%s;\s\+$;;e'
                \| call winrestview(s:stay_view)
                \| let @/ = s:old_search
                \| unlet s:old_search
                \| unlet s:stay_view

    " remember last cursor position in buffer
    autocmd BufReadPost *
                \  if line("'\"") > 0 && line("'\"") <= line('$')
                \|     execute 'norm! g`"zvzz'
                \| endif

    " cursor line
    "autocmd WinLeave,InsertEnter * set nocursorline
    "autocmd WinEnter,InsertLeave * set cursorline

    " swapchoice
    autocmd SwapExists * let v:swapchoice = 'o'

    " save when losing focus
    autocmd FocusLost * :silent! wall

    " auto make dir
    autocmd BufWritePre * :call <sid>make_non_ex_dir(expand('<afile'), +expand('<abuf>'))
    func! s:make_non_ex_dir(file, buf)
        if empty(getbufvar(a:buf, '&buftype')) && a:file!~#'\v^\w+\:\/'
            let dir=fnamemodify(a:file, ':h')
            if !isdirectory(dir)
                call mkdir(dir, 'p')
            endif
        endif
    endfunc

    " resize splits when the window is resized
    "autocmd VimResized * :wincmd =

    if 0 && s:is_term_macvim
    "   " retain input sorce for US in normal mode
    "   " https://github.com/myshov/xkbswitch-macosx
    "   " get: xkbswitch -ge
        let lastxkb = 0
        autocmd InsertLeave * :let lastxkb = system(s:dot_vim_path.'/etc/xkbswitch -gn') | call system(s:dot_vim_path.'/etc/xkbswitch -sn 0')
        autocmd InsertEnter * :if lastxkb != 0 | call system(s:dot_vim_path.'/etc/xkbswitch -sn ' . lastxkb) | endif
        "autocmd InsertLeave * :call system(s:dot_vim_path.'/etc/xkbswitch -se US')
    endif
augroup END

autocmd BufNewFile,BufRead *.org setf org
autocmd BufNewFile,BufRead *.adoc setf asciidoc

autocmd FileType javascript setlocal tabstop=2
"}}}

"settings {{{
set tabstop=4 shiftwidth=0 softtabstop=-1
set expandtab shiftround
set linespace=3

set list
set listchars=tab:\|\ ,trail:@,extends:>,precedes:<
set fillchars=vert:\|,diff:-

if has('patch-7.4.352')
    set breakindent "breakindentopt
endif
set linebreak
set wrap display=lastline
let &showbreak='+>>> '

set backspace=2
set autoindent cindent
set showmode showcmd novisualbell
"set cmdheight=2

set colorcolumn=80,130
set title
set nonumber norelativenumber numberwidth=3
set ruler
set laststatus=2

set scrolloff=2 sidescroll=1 sidescrolloff=16
set showmatch matchpairs+=<:> matchtime=1
set nostartofline

set hlsearch
set ignorecase smartcase incsearch wrapscan
set gdefault

set virtualedit=block,insert

set foldmethod=marker
"set foldopen-=search
set foldcolumn=2
set foldlevelstart=-1

set timeout ttimeout
set timeoutlen=512 ttimeoutlen=0
set updatetime=5000 updatecount=100

set complete=.,w,b,u,t
set completeopt=longest,menuone,preview
set infercase

set undofile undoreload=10000
set backup noswapfile
let [&undodir, &backupdir, &directory] =
            \[s:undodir_path, s:backupdir_path, s:swapdir_path]

set history=1024

set pumheight=10
set wildmenu wildmode=full wildchar=<tab>
set wildignore+=.hg,.git,.svn
set wildignore+=*.jpg,*.bmp,*.gif,*.png,*.jpeg
set wildignore+=*.o,*.obj,*.exe,*.dll,*.manifest
set wildignore+=*.DS_Store
set wildignore+=*.luac,*.pyc
set wildignore+=*.meta

set fileformats=unix,dos,mac
set clipboard=unnamed,unnamedplus ",autoselect

set hidden
set mouse=
"set switchbuf=useopen

set iminsert=0 imsearch=0 "imdisable

"set dictionary=/usr/share/dict/words
"set spellfile=~/dotfiles/vim/custom-dictionary.utf-8.add
"nnoremap zG 2zg

syntax sync fromstart
"}}}

"normal key mapping {{{
let mapleader = '\\'
let maplocalleader = '\\'

cnoremap <c-a> <home>
cnoremap <c-e> <end>

inoremap <c-a> <home>
inoremap <c-e> <end>
inoremap <c-f> <c-o>w
inoremap <c-b> <c-o>b
inoremap <c-d> <del>
inoremap <c-h> <left>
inoremap <c-j> <down>
inoremap <c-k> <up>
inoremap <c-l> <right>

"inoremap <c-f> <c-x><c-f>
"inoremap <c-]> <c-x><c-]>
"inoremap <c-l> <c-x><c-l>
inoremap <c-u> <c-g>u<c-u>

noremap ; :
noremap : ;

inoremap jj <esc>
inoremap ㅓ <esc>
nnoremap <esc><esc> :noh<cr>

noremap j gj
noremap k gk
noremap gj j
noremap gk k

nnoremap Y yg_

noremap <c-h> <c-w>h
noremap <c-j> <c-w>j
noremap <c-k> <c-w>k
noremap <c-l> <c-w>l

nmap <c-w>+ <c-w>+<c-w>
nmap <c-w>- <c-w>-<c-w>
nmap <c-w>> <c-w>><c-w>
nmap <c-w><lt> <c-w><<c-w>

nnoremap g; g;zz
nnoremap g, g,zz

vnoremap <silent> y y`]
vnoremap <silent> p p`]
nnoremap <silent> p p`]

vnoremap <tab> >gv
vnoremap <s-tab> <gv

nnoremap gV `[v`]
nnoremap vv ^vg_

nnoremap <f5> :silent execute '%y\|vne\|pu!"'<cr>:%!bash<cr>
vnoremap <space>S y:@"<cr>
nnoremap <space>S ^vg_y:exe @@<cr>:echo 'Sourced line.'<cr>

nnoremap <silent> *
            \ :let stay_view = winsaveview()<cr>*:call winrestview(stay_view)<cr>

nnoremap <silent> <space>/ :execute "vimgrep /".@/."/g %"<cr>:copen<cr>
nnoremap <space>a :Ack!<space>
nnoremap <space>ls :set list!<cr>

"nnoremap <space>z mzzMzvzz15<c-e>`z:Pulse<cr>
nnoremap <space>z zMzv:Pulse<cr>

nnoremap <space>lc :lc %:p:h<cr>
nnoremap <space>cd :cd %:p:h<cr>

nnoremap <space>dic :silent !start C:\Program Files (x86)\Google\Chrome\Application\chrome.exe http://endic.naver.com/search.nhn?sLn=en&isOnlyViewEE=N&query=<cword><cr>

if s:is_company
    nnoremap <space>sl :silent !start TortoiseProc.exe /command:log /path:%<cr>
    nnoremap <space>sd :silent !start TortoiseProc.exe /command:diff /path:%<cr>
    nnoremap <space>sb :silent !start TortoiseProc.exe /command:blame /path:%<cr>

    " old settings
    "nn <space>gr :gr **/*.py<home><right><right><space>
    "nn <space>ss :new \| se ft=diff \| r!svn diff #<cr>:setl nomodified \| nn <buffer> q ZQ<cr>gg
    "nn <space>sd :if bufname('%') != ''\|new\|en<cr>:%d \| se ft=diff \| r!svn diff<cr>:setl nomodified \| nn <buffer> q ZQ<cr>gg
    "au BufEnter *.py se ts=4 sw=4 sts=0 noet
    "au BufEnter test_client.py :nn <f5> :%d\|r!python test/test_client.py
endif

nnoremap <space>n <esc>:Lexplore \| vertical resize 24<cr>
nnoremap <space>nc <esc>:Lexplore %:p:h \| vertical resize 24<cr>

nnoremap <space>p <esc>:CtrlPCurWD<cr>
nnoremap <space>pp <esc>:CtrlPBuffer<cr>
nnoremap <space>p[ <esc>:CtrlPMRUFiles<cr>
nnoremap <space>pt <esc>:CtrlPTag<cr>
nnoremap <space>ptt <esc>:CtrlPBufTag<cr>

noremap <space>ww :CtrlP ~/doc<cr>

nnoremap <space>t <esc>:Tlist<cr>

nnoremap <space>q <esc>:%norm<space>
vnoremap <space>q :norm<space>

vnoremap <silent> s //e<c-r>=&selection=='exclusive'?'+1':''<cr><cr>
                    \:<c-u>call histdel('search',-1)<bar>let @/=histget('search',-1)<cr>gv
"}}}

"mini-plugin {{{

" short_open {{{
nnoremap <silent> <space>e :call <sid>short_open()<cr>
func! s:short_open()
    call s:popup_hint(['[v]imrc', '[h]osts', '[t]odo', '[d]ev'])

    let c = nr2char(getchar())
    if c ==# 'v'
        let p = s:vimrc_path
    elseif c ==# 'h'
        let p = expand('C:\Windows\System32\drivers\etc\hosts')

    elseif c ==# 't'
        call <sid>simple_note_adapter('o', '0966f305906111e6be8883f1d7a5c9bd')
        return
    elseif c ==# 'd'
        call <sid>simple_note_adapter('o', '83c466dd1b98f9c1c387cd9fa89ec475')
        return
    else
        return
    endif

    execute 'tabedit' p
endfunc
" }}}

" visual_set_seartch {{{
vnoremap * :<c-u>call <sid>visual_set_search()<cr>//<cr><c-o>
vnoremap # :<c-u>call <sid>visual_set_search()<cr>??<cr><c-o>
func! s:visual_set_search()
  let temp = @@
  norm! gvy
  let @/ = '\V' . substitute(escape(@@, '/\'), '\n', '\\n', 'g')
  let @@ = temp
endfunc
"}}}

" split_line {{{
nnoremap <space>J :<c-u>call <sid>split_line()<cr>
func! s:split_line()
    let [old_search, stay_view] = [@/, winsaveview()]
    execute "normal! i\<cr>\<esc>gk"
    silent! 's;\s+$;;'
    call winrestview(stay_view)
    let @/ = old_search
endfunc
" }}}

" simple_note_adapter {{{
nnoremap <silent> <space>ol :call <sid>simple_note_adapter('o', 'ca0b5a7db50211e5a3d06384bf57b787')<cr>
nnoremap <silent> <space>o :call <sid>simple_note_adapter()<cr>
func! s:simple_note_adapter(...)
    if !exists('g:loaded_simplenote_vim')
        let g:SimplenoteUsername=$VIM_SIMPLENOTE_EMAIL
        let g:SimplenotePassword=inputsecret('simplenote password:', '')

        if g:SimplenotePassword == ''
            return
        endif
    endif

    if a:0 == 2 && a:1 == 'o'
        execute 'call simplenote#SimplenoteOpen("'.a:2.'")'
    else
        execute 'call simplenote#SimplenoteList()'
    endif

    if exists('g:SimplenotePassword')
        unlet g:SimplenotePassword
    endif
endfunc
"}}}

" short_input {{{
inoremap <silent> <c-g> <c-o>:call <sid>short_input()<cr>
func! s:short_input()
    call s:popup_hint(['dt', 't', 'dty', 'dy', 'U', 'calc'])

    let backup = @z
    let in = input('')

    if in ==# 'dt'
        let @z = strftime('%Y-%m-%d %H:%M:%S')

    elseif in ==# 't'
        let @z = strftime('%H:%M:%S')

    elseif in ==# 'dty'
        let @z = strftime('%Y-%m-%d ')
        let @z .= ["SU","MO","TU","WE","TH","FR","SA"][strftime('%w')]
        let @z .= strftime(' %H:%M:%S')

    elseif in ==# 'dy'
        let @z = strftime('%Y-%m-%d ')
        let @z .= ["SU","MO","TU","WE","TH","FR","SA"][strftime('%w')]

    elseif in ==# 'U'
        normal mzgUiw`za
        return

    elseif in ==# 'calc'
        execute "normal! yiWA=\<c-r>=\<c-r>0\<cr>"
        "개선필요
        return
    else
        return
    endif

    normal "zP
    let @z = backup
endfunc
"}}}

"}}}

"command{{{
command! -range JsonFormat '<,'>!python -m json.tool
command! -range -nargs=* Sh <line1>,<line2>!cd %:p:h:gs?\\?/?; <args>
command! -range -nargs=* Shr <line1>,<line2>r!cd %:p:h:gs?\\?/?; <args>

func! s:open_win_explorer()
    let cwd = expand('%:p:h')
    echo cwd
    silent! call system("explorer " . cwd)
endfunc
command! OE call <sid>open_win_explorer()
"}}}

"abbreviate{{{
abbreviate fucntion function
abbreviate calss class
abbreviate laudump luadump
abbreviate plcaed placed
"}}}

"function {{{
command! -nargs=0 Pulse call <sid>Pulse()
func! s:Pulse()
    redir => old_hi
        silent execute 'hi CursorLine'
    redir END
    let old_hi = split(old_hi, '\n')[0]
    let old_hi = substitute(old_hi, 'xxx', '', '')

    let steps = 8
    let width = 1
    let start = width
    let end = steps * width
    let color = 233

    for i in range(start, end, width)
        execute "hi CursorLine ctermbg=" . (color + i) . " guibg=#666666"
        redraw
        sleep 6m
    endfor
    for i in range(end, start, -1 * width)
        execute "hi CursorLine ctermbg=" . (color + i) . " guibg=#666666"
        redraw
        sleep 6m
    endfor

    execute 'hi ' . old_hi
endfunc

command! -nargs=+ -complete=command TabMessage call <sid>TabMessage(<q-args>)
func! s:TabMessage(cmd)
  redir => message
  silent execute a:cmd
  redir END
  tabnew
  silent put=message
  set nomodified
endfunc


func! s:SourceRange() range
 let tmpsofile = tempname()
 call writefile(getline(a:firstline, a:lastline), l:tmpsofile)
 execute "source " . l:tmpsofile
 call delete(l:tmpsofile)
endfunc
command! -range Source <line1>,<line2>call <sid>SourceRange()


func! s:popup_hint(hints)
    let width = 0

    for h in a:hints
        if strlen(h) > width
            let width = strlen(h)
        endif
    endfor

    let width_unit = 12
    let width += width_unit - width % width_unit

    let [out, linewidth, maxwidth] = ['', 0, winwidth(0)]

    for h in sort(a:hints)
        if linewidth + width >= maxwidth
            let linewidth = 0
            let out .= '\n'
        endif

        let linewidth += width
        let out .= h . repeat(' ', width - strlen(h))
    endfor

    redraw
    echo out
    echo ''
endfunc

command! EchoSyntax
            \ for id in synstack(line('.'), col('.'))
            \| echomsg synIDattr(id, 'name')
            \| endfor
"}}}
