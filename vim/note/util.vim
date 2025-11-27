" note/util.vim - 공용 유틸리티 함수

if exists('g:loaded_note_util')
  finish
endif
let g:loaded_note_util = 1

" marker 파일로 root 탐색
function! NoteRootByMarker(start_dir, marker) abort
  let dir = a:start_dir
  while dir != '/'
    if filereadable(dir . '/' . a:marker)
      return dir
    endif
    let dir = fnamemodify(dir, ':h')
  endwhile
  return ''
endfunction

" 상대 경로 계산 (ruby 사용)
function! NoteRelativePath(target, from_dir) abort
  return trim(system("ruby -e \"require 'pathname'; puts Pathname.new('" . a:target . "').relative_path_from(Pathname.new('" . a:from_dir . "'))\""))
endfunction

" 확장자 유무 판단
function! NoteHasExtension(path) abort
  return a:path =~ '\.[^./]\+$'
endfunction
