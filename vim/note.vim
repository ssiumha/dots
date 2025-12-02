let g:neovide_transparency = 0.97
let g:neovide_scale_factor = 1.4

" set guifont=Menlo:h14
set linespace=4
set nocursorcolumn
set conceallevel=2
set concealcursor=nci
set splitright
color spacemacs-theme

" lightweight
set indentexpr=
set formatoptions-=at
set synmaxcol=200
silent! NoMatchParen
let g:ale_lint_on_text_changed = 'never'
let g:ale_lint_on_insert_leave = 1
let g:ale_lint_on_enter = 0
" lightweight end

nnoremap <D-v> "+p
inoremap <D-v> <c-r>+
tnoremap <D-v> <c-r>+
cnoremap <D-v> <c-r>+

let g:slime_target = "vimterminal"
let g:slime_vimterminal_cmd = "/bin/zsh"
let g:slime_vimterminal_config = {"term_finish": "close"}

let s:session_path = '~/.local/vim/neovide_session.vim'
let s:default_dir = '~/org'
let s:default_file = 'index.md'

function! RestoreOrDefault()
  execute 'cd ' . s:default_dir
  " if filereadable(expand(s:session_path))
    " execute 'source ' . s:session_path
  " else
    execute 'edit ' . s:default_file
  " endif
endfunction

autocmd VimLeave * mksession! ~/.local/vim/neovide_session.vim
autocmd VimEnter * ++nested if argc() == 0 && !exists("s:std_in") | call RestoreOrDefault() | endif

let g:timeline_perl =<< trim EOF
if (/^@{1,2}(\d{3}-\d{2}-\d{2})\b/) {
  use Time::Piece; use Time::Seconds;
  my $d = int( (Time::Piece->strptime($1,"%Y-%m-%d") - localtime) / ONE_DAY );
  if (s/\s*\(D-?\d+\)\s*:/ " (D-$d):"/e) { }
  else { s/:/ " (D-$d):"/e; }
}
EOF
function! TimelineUpdateVisual() range abort
  let l:first = a:firstline
  let l:last  = a:lastline
  execute l:first . ',' . l:last . '!perl -pe ' . shellescape(g:timeline_perl)
endfunction
xnoremap <silent> <space>td :<C-u>call TimelineUpdateVisual()<CR>

" call FloatermCmd(g:sh_insert_link, { out -> nvim_put(out, 'c', v:true, v:true) })
let g:sh_insert_link =<< trim EOF
link=$(fzf --prompt='LINK> ' --bind 'enter:become:[ -z {} ] && echo {q} || echo {}')
[ -z $link ] && exit 1
title=$(echo $link | fzf --prompt='TITLE> ' --bind 'enter:become:echo {q}')
echo "[$title]($link)"
EOF

" call FloatermCmd(g:open_or_create_resources, { out -> append(line('.'), out) })
"nnoremap <space>or :call FloatermCmd(g:open_or_create_resources, { out -> empty(out) ? '' : execute('tabe ' . join(out, '')) })<cr>
"let g:open_or_create_resources =<< trim EOF
"doc=$(fd . --base-directory ~/org/resources -t f \
"  | fzf --bind 'enter:become:[ -z {} ] && echo {q} || echo {}' \
"        --bind 'ctrl-o:become:touch ~/org/resources/{q} && echo {q}' \
"        --header 'ctrl-o: new file' \
")
"[ $? -ne 0 ] && exit 1
"[ -z $doc ] && exit 1
"echo "~/org/resources/$doc"
"EOF


" :echo system(join(g:open_link, "\n"), getline('.'))
" my ($path, $name) = split /\s+/, <>;
" print "PATH=$path NAME=$name\n";
let g:open_link =<< trim EOF
perl -ne 'print uc'
EOF

let g:markdown_fenced_languages = [
  \ 'javascript',
  \ 'typescript',
  \ 'python',
  \ 'ruby',
  \ 'bash=sh',
  \ ]

function! InMarkdownCodeSyntax(lnum) abort
  let ids = synstack(a:lnum, 1)
  if empty(ids) | return 0 | endif
  for id in ids
  if synIDattr(id, 'name') ==# 'markdownCodeBlock'
      return 1
    endif
  endfor
  return 0
endfunction

function! MarkdownFoldExpr() abort
  let l = getline(v:lnum)
  if l =~# '^#\+\s'
    return '>' . strlen(matchstr(l, '^#\+'))
  endif
  if l =~# '^\s*\(```\|~~~\)'
    return '='
  endif
  if InMarkdownCodeSyntax(v:lnum)
    return '='
  endif
  return '='
endfunction

" WebDAV 링크 파싱: server:path 형식 감지
function! ParseWikiLink(link)
  " @로 시작하면 self-nested 링크
  if a:link =~ '^@'
    return {
      \ 'type': 'self',
      \ 'path': a:link[1:]
    \ }
  endif

  " server:path 형식인지 체크 (path는 /로 시작)
  let parts = split(a:link, ':', 1)
  if len(parts) >= 2 && parts[1] =~ '^/'
    return {
      \ 'type': 'webdav',
      \ 'server': parts[0],
      \ 'path': join(parts[1:], ':')
    \ }
  else
    return {'type': 'local', 'path': a:link}
  endif
endfunction

" Extract URL from markdown link [text](url) format
function! ExtractMarkdownLinkUrl()
  let line = getline('.')
  let col = col('.') - 1

  " Find [text](url) pattern around cursor
  let start = match(line, '\[.\{-}\](', 0)
  while start >= 0 && start <= col
    let end = match(line, ')', start)
    if end > col
      " Cursor is inside this link
      let url_start = match(line, '(', start) + 1
      return line[url_start : end-1]
    endif
    let start = match(line, '\[.\{-}\](', end)
  endwhile

  return ''
endfunction

function! s:ExtractWikilinkFull()
  let [line, col, start] = [getline('.'), col('.') - 1, 0]
  while 1
    let [ms, me] = [match(line, '\[\[', start), match(line, '\]\]', start)]
    if ms == -1 || me == -1 | break | endif
    if col >= ms && col <= me + 1
      let content = line[ms + 2 : me - 1]
      let p = stridx(content, '|')
      return p != -1
        \ ? {'found': 1, 'target': content[:p-1], 'alias': content[p+1:], 'start': ms, 'end': me + 2}
        \ : {'found': 1, 'target': content, 'alias': '', 'start': ms, 'end': me + 2}
    endif
    let start = me + 2
  endwhile
  return {'found': 0}
endfunction

function! s:ExtractMarkdownLinkFull()
  let [line, col, start] = [getline('.'), col('.') - 1, 0]
  while 1
    let ms = match(line, '\[', start)
    if ms == -1 | break | endif
    let be = match(line, '\]', ms)
    if be == -1 | break | endif
    if be + 1 < len(line) && line[be + 1] == '('
      let pe = match(line, ')', be + 2)
      if pe != -1 && col >= ms && col <= pe
        return {'found': 1, 'title': line[ms+1:be-1], 'url': line[be+2:pe-1], 'start': ms, 'end': pe + 1}
      endif
    endif
    let start = be + 1
  endwhile
  return {'found': 0}
endfunction

function! InsertWikilink() abort
  let target = input('Target: ')
  if empty(target) | return | endif
  let alias = input('Alias: ')
  exe 'normal! a' . (empty(alias) ? '[['.target.']]' : '[['.target.'|'.alias.']]')
endfunction

function! InsertMarkdownLink() abort
  let url = input('URL: ')
  if empty(url) | return | endif
  let title = input('Title: ', url)
  exe 'normal! a[' . title . '](' . url . ')'
endfunction

function! EditLinkAtCursor() abort
  let w = s:ExtractWikilinkFull()
  if w.found
    let t = input('Target: ', w.target)
    if empty(t) | return | endif
    let a = input('Alias: ', w.alias)
    if empty(a) && !empty(w.alias) | return | endif
    let link = empty(a) ? '[['.t.']]' : '[['.t.'|'.a.']]'
    call setline('.', getline('.')[0:w.start-1] . link . getline('.')[w.end:])
    return
  endif

  let m = s:ExtractMarkdownLinkFull()
  if m.found
    let u = input('URL: ', m.url)
    if empty(u) | return | endif
    let t = input('Title: ', m.title)
    if empty(t) | return | endif
    let link = '[' . t . '](' . u . ')'
    call setline('.', getline('.')[0:m.start-1] . link . getline('.')[m.end:])
    return
  endif

  echo "No link under cursor"
endfunction

function! InsertLink() abort
  echo "Link: [w]iki [m]arkdown"
  let c = nr2char(getchar()) | redraw
  if c ==# 'w' | call InsertWikilink()
  elseif c ==# 'm' | call InsertMarkdownLink()
  endif
endfunction

function! SmartLink() abort
  if s:ExtractWikilinkFull().found || s:ExtractMarkdownLinkFull().found
    call EditLinkAtCursor()
  else
    call InsertLink()
  endif
endfunction

runtime note/util.vim
runtime note/dataview.vim
runtime note/outline.vim
runtime note/move.vim

function! GetMarkdownPagePath()
  let syn_name = synIDattr(synID(line('.'), col('.'), 1), 'name')

  " WikiLink: [[link]]
  if syn_name ==# 'markdownWikiLink'
    let link = substitute(expand('<cWORD>'), '^\[\[\([^]|]\+\).*$', '\1', '')
    return ParseWikiLink(link)
  endif

  " Markdown Link: [text](url)
  if syn_name =~# 'markdown.*Link\|markdownUrl'
    let url = ExtractMarkdownLinkUrl()
    if !empty(url)
      return ParseWikiLink(url)
    endif
  endif

  return {'type': 'none'}
endfunction

function! OpenWiki() abort
  let link_info = GetMarkdownPagePath()

  if link_info.type == 'webdav'
    " WebDAV 파일 열기
    tabnew
    call webdav#file#get(link_info.path, link_info.server)
  elseif link_info.type == 'self'
    " Self-nested: 현재 파일명 폴더 안 문서
    let base = expand('%:p:r')
    let page = base . '/' . link_info.path
    " 확장자 없으면 .md 추가
    if page !~ '\.[^./]\+$'
      let page .= '.md'
    endif
    execute 'tabe' fnameescape(page)
  elseif link_info.type == 'local'
    " 기존 로컬 파일 열기
    let page = link_info.path
    if page !~ '/$'
      " 확장자 없으면 .md 추가
      if page !~ '\.[^./]\+$'
        let page .= '.md'
      endif
    endif
    let page = page =~# '^\v(/|\~|\w+:)' ? page : expand('%:p:h') . '/' . page
    execute 'tabe' fnameescape(page)
  else
    normal! <CR>
  endif
endfunction

" FZF로 로컬 파일 탐색 → wikilink 삽입
function! LocalLinkFzf() abort
  " 우선순위: git root → index.md marker → 현재 디렉토리
  let git_root = trim(system('git rev-parse --show-toplevel 2>/dev/null'))
  let marker_root = NoteRootByMarker(expand('%:p:h'), 'index.md')
  let base_dir = !empty(git_root) ? git_root : (!empty(marker_root) ? marker_root : expand('%:p:h'))
  let current_file = expand('%:p')

  let cmd = 'fd -t f -e md --base-directory ' . shellescape(base_dir)

  call fzf#run(fzf#wrap({
    \ 'source': cmd,
    \ 'sink*': function('s:HandleLinkSelection', [base_dir, current_file]),
    \ 'options': [
    \   '--prompt', '[[',
    \   '--print-query',
    \   '--expect', 'ctrl-n',
    \   '--preview', 'head -20 {}',
    \   '--header', 'ctrl-n: 날짜 노트'
    \ ],
    \ 'down': '40%'
  \ }))
endfunction

function! s:HandleLinkSelection(base_dir, current_file, result) abort
  if len(a:result) < 1 | return | endif

  let query = a:result[0]
  let key = len(a:result) > 1 ? a:result[1] : ''
  let selection = len(a:result) > 2 ? a:result[2] : ''

  " ctrl-n: 쿼리 + /날짜 형태 링크 생성
  if key ==# 'ctrl-n'
    let path = trim(query)
    if empty(path) | return | endif
    let link = path . '/' . strftime('%Y-%m-%d')
    execute "normal! a[[" . link . "]]"
    startinsert!
    return
  endif

  " 일반 선택 또는 직접 입력
  if !empty(selection)
    let current_dir = fnamemodify(a:current_file, ':h')
    let target = a:base_dir . '/' . selection
    let relative = NoteRelativePath(target, current_dir)
    let relative = substitute(relative, '\.md$', '', '')
    let link = relative
  else
    let link = query
  endif

  if empty(link) | return | endif

  execute "normal! a[[" . link . "]]"
  startinsert!
endfunction

augroup MyMarkdown
  autocmd!
  autocmd FileType markdown silent! TableModeEnable
  autocmd FileType markdown setlocal foldmethod=expr foldexpr=MarkdownFoldExpr() foldenable
  autocmd FileType markdown setlocal foldtext=getline(v:foldstart).'...'
  autocmd FileType markdown nnoremap <buffer> <tab> za
  autocmd FileType markdown nnoremap <buffer> <cr> :call OpenWiki()<cr>
  autocmd FileType markdown nnoremap <buffer> <c-c><c-l> :call SmartLink()<cr>
  autocmd FileType markdown inoremap <buffer> <C-c><C-l> <C-o>:call SmartLink()<CR>
  autocmd FileType markdown nnoremap <buffer> <c-c><c-l> :call SmartLink()<cr>
  autocmd FileType markdown nnoremap <buffer> <c-c><c-p> :call DataviewProperty()<cr>
  autocmd FileType markdown nnoremap <buffer> <c-c><c-o> :call MarkdownOutline()<cr>
  autocmd FileType markdown nnoremap <buffer> <c-c><c-w> :MoveFileFzf<cr>

  autocmd FileType markdown nnoremap <buffer> <C-c>. :PickDateAtCursor<CR>
  autocmd FileType markdown inoremap <buffer> <C-c>. <c-o>:PickDateAtCursor<CR>

  autocmd FileType markdown inoremap <buffer> [[ <C-o>:call LocalLinkFzf()<CR>

  autocmd FileType markdown noremap <S-Up>    :call ShiftDateAtCursor(1)<CR>
  autocmd FileType markdown noremap <S-Down>  :call ShiftDateAtCursor(-1)<CR>
  autocmd FileType markdown noremap <S-Left>  :call ShiftDateAtCursor(-1, 'day')<CR>
  autocmd FileType markdown noremap <S-Right> :call ShiftDateAtCursor(1, 'day')<CR>

  autocmd InsertEnter *.md setlocal foldmethod=manual
  autocmd InsertLeave *.md setlocal foldmethod=expr

  autocmd VimLeavePre *.md mkview
  autocmd FileType markdown silent! loadview

  " autocmd FileType markdown nnoremap <buffer> <leader>fc zM
  " autocmd FileType markdown nnoremap <buffer> <leader>fa zR

  autocmd FileType markdown ++once {
    hi Folded gui=bold guifg=#ff5f00 guibg=NONE
    hi markdownH1 gui=bold guifg=#e6b422
    hi markdownH2 gui=bold guifg=#d75f5f
    hi markdownH3 gui=bold guifg=#5f87d7
    hi markdownH4 gui=NONE guifg=#5faf5f
    hi markdownH5 gui=NONE guifg=#af5fd7
    hi markdownH6 gui=NONE guifg=#aaaaaa
    hi markdownCodeBlock guibg=#1e1e2e
    hi markdownFencedCodeBlock guibg=#1e1e2e
    hi markdownDone guifg=#777777
    hi markdownDelegated guifg=#777777
    hi markdownStrike gui=strikethrough
    hi MyTodo guifg=#000000 guibg=#FFD700 gui=bold
    hi MyDone guifg=#000000 guibg=#98FB98 gui=bold
    hi link markdownWikiLink markdownLinkText
    hi Conceal guifg=#bc6ec5 guibg=#292b2e
    hi markdownDataviewField guifg=#888888 gui=italic
  }

  autocmd Syntax markdown {
    syntax match MyTodo /\<TODO\>/
    syntax match MyDone /\<DONE\>/
    syntax match markdownWikiLink /\[\[.*\]\]/
    syntax match markdownDone /\v^\s*-\s\[x\]\s.*$/
    syntax match markdownDelegated /\v^\s*-\s\[\>\]\s.*$/ contains=markdownStrike

    syntax match markdownDataviewField /\[[^\[\]]*::[^\[\]]*\]/ contains=markdownDataviewDelim
    syntax match markdownDataviewDelim /\[\|\]/ contained conceal
  }

  autocmd Syntax markdown syntax clear markdownLink
  autocmd Syntax markdown syntax region markdownLink
        \ matchgroup=markdownLinkDelimiter
        \ start="(" end=")"
        \ contains=markdownUrl
        \ conceal keepend contained cchar=🔗
augroup END
