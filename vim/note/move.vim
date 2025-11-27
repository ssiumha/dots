" MoveFileFzf - FZF로 파일 이동 + wikilink 업데이트
" Requires: fzf plugin, fd, note/util.vim

if exists('g:loaded_note_move')
  finish
endif
let g:loaded_note_move = 1

" wikilink를 실제 파일 경로로 해석
function! s:ResolveWikilink(from_dir, link, base_dir) abort
  let path = a:link

  " 확장자 없으면 .md 추가
  if !NoteHasExtension(path)
    let path .= '.md'
  endif

  " 상대 경로면 from_dir 기준으로 해석
  if path !~ '^/'
    let candidate = simplify(a:from_dir . '/' . path)
    if filereadable(candidate)
      return candidate
    endif
  endif

  " 절대 경로 (base_dir 기준)
  let candidate = simplify(a:base_dir . '/' . path)
  if filereadable(candidate)
    return candidate
  endif

  return ''
endfunction

function! MoveFileFzf() abort
  let current_file = expand('%:p')
  if empty(current_file) || !filereadable(current_file)
    echo "No file to move"
    return
  endif

  " root 탐색: git root → index.md marker → 현재 디렉토리
  let git_root = trim(system('git rev-parse --show-toplevel 2>/dev/null'))
  let marker_root = NoteRootByMarker(expand('%:p:h'), 'index.md')
  let base_dir = !empty(git_root) ? git_root : (!empty(marker_root) ? marker_root : expand('%:p:h'))

  " fd로 폴더 스캔
  let cmd = 'fd -t d --base-directory ' . shellescape(base_dir)

  call fzf#run(fzf#wrap({
    \ 'source': cmd,
    \ 'sink*': function('s:HandleMoveSelection', [base_dir, current_file]),
    \ 'options': ['--prompt', 'Move to> ', '--print-query', '--header', 'Enter: select folder | Query: create new folder'],
    \ 'down': '40%'
  \ }))
endfunction

function! s:HandleMoveSelection(base_dir, current_file, result) abort
  if len(a:result) < 1 | return | endif

  let query = a:result[0]
  let selection = len(a:result) > 1 ? a:result[1] : ''
  let target_dir = !empty(selection) ? (a:base_dir . '/' . selection) : (a:base_dir . '/' . query)

  if empty(target_dir) || target_dir == a:base_dir
    echo "No folder selected"
    return
  endif

  " trailing slash 제거
  let target_dir = substitute(target_dir, '/$', '', '')

  " 새 폴더면 생성
  if !isdirectory(target_dir)
    call mkdir(target_dir, 'p')
    echo "Created: " . target_dir
  endif

  let filename = fnamemodify(a:current_file, ':t')
  let new_path = target_dir . '/' . filename

  " 덮어쓰기 확인
  if filereadable(new_path)
    let confirm = input("File exists. Overwrite? (y/N): ")
    if confirm !~? '^y'
      echo "\nCancelled"
      return
    endif
  endif

  " wikilink 업데이트 (이동 전에 수행)
  call s:UpdateWikilinksForMove(a:base_dir, a:current_file, new_path)

  " 파일 이동
  call rename(a:current_file, new_path)

  " 버퍼 업데이트
  execute 'edit ' . fnameescape(new_path)
  execute 'bwipeout ' . bufnr(a:current_file)

  echo "Moved to: " . new_path
endfunction

function! s:UpdateWikilinksForMove(base_dir, old_path, new_path) abort
  let files = systemlist('fd -t f -e md --base-directory ' . shellescape(a:base_dir))

  for relfile in files
    let file = a:base_dir . '/' . relfile
    if file == a:old_path | continue | endif

    let lines = readfile(file)
    let file_dir = fnamemodify(file, ':h')
    let file_modified = 0

    for i in range(len(lines))
      let line = lines[i]
      let newline = ''
      let lastend = 0
      let start = 0
      let line_modified = 0

      while 1
        " [[...]] 또는 [...](...) 중 먼저 나오는 것 찾기
        let wiki_ms = match(line, '\[\[', start)
        let md_ms = match(line, '\[[^\[\]]*\](', start)

        if wiki_ms == -1 && md_ms == -1 | break | endif

        " 더 앞에 있는 것 선택
        if wiki_ms != -1 && (md_ms == -1 || wiki_ms < md_ms)
          " === Wikilink 처리: [[target|alias]] ===
          let me = match(line, '\]\]', wiki_ms)
          if me == -1 | break | endif

          let content = line[wiki_ms+2 : me-1]
          let parts = split(content, '|', 1)
          let target = parts[0]
          let alias = len(parts) > 1 ? parts[1] : ''

          let resolved = s:ResolveWikilink(file_dir, target, a:base_dir)

          if resolved == a:old_path
            let new_rel = NoteRelativePath(a:new_path, file_dir)
            " 원래 링크에 확장자가 없었으면 .md 제거
            if !NoteHasExtension(target)
              let new_rel = substitute(new_rel, '\.md$', '', '')
            endif
            let newlink = empty(alias) ? '[[' . new_rel . ']]' : '[[' . new_rel . '|' . alias . ']]'
            let newline .= line[lastend : wiki_ms-1] . newlink
            let line_modified = 1
          else
            let newline .= line[lastend : me+1]
          endif
          let lastend = me + 2
          let start = me + 2

        elseif md_ms != -1
          " === Markdown link 처리: [text](path) ===
          let text_end = match(line, '\](', md_ms)
          if text_end == -1 | break | endif
          let path_start = text_end + 2
          let path_end = match(line, ')', path_start)
          if path_end == -1 | break | endif

          let text = line[md_ms+1 : text_end-1]
          let path = line[path_start : path_end-1]

          let resolved = s:ResolveWikilink(file_dir, path, a:base_dir)

          if resolved == a:old_path
            let new_rel = NoteRelativePath(a:new_path, file_dir)
            let newlink = '[' . text . '](' . new_rel . ')'
            let newline .= line[lastend : md_ms-1] . newlink
            let line_modified = 1
          else
            let newline .= line[lastend : path_end]
          endif
          let lastend = path_end + 1
          let start = path_end + 1
        endif
      endwhile

      if line_modified
        let lines[i] = newline . line[lastend :]
        let file_modified = 1
      endif
    endfor

    if file_modified
      call writefile(lines, file)
      echo "Updated: " . file
    endif
  endfor
endfunction

command! MoveFileFzf call MoveFileFzf()
