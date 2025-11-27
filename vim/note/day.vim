" Date picker using floaterm + day script
" Requires: floaterm plugin, ~/dots/bin/day

if exists('g:loaded_note_day')
  finish
endif
let g:loaded_note_day = 1

let s:pick_callback = v:null
let s:pick_tmpfile = ''
let s:pick_include_weekday = 0
let s:return_to_insert = 0

function! PickDate(initial_date, Callback, ...) abort
  let date = empty(a:initial_date) ? strftime('%Y-%m-%d') : a:initial_date
  let include_weekday = a:0 >= 1 ? a:1 : 0
  let s:pick_tmpfile = tempname()
  let s:pick_callback = a:Callback
  let s:pick_include_weekday = include_weekday

  " tee로 stdout을 tempfile에 저장하면서 터미널에도 출력
  let weekday_flag = include_weekday ? '-w' : '--no-weekday'
  let cmd = printf('%s/dots/bin/day %s %s | tee %s', $HOME, weekday_flag, date, s:pick_tmpfile)
  let jobopts = {'on_exit': function('s:OnPickDateExit')}
  call floaterm#terminal#open(-1, [&shell, &shellcmdflag, cmd], jobopts, {})
endfunction

function! s:OnPickDateExit(job, data, event, ...) abort
  if s:pick_callback is v:null
    return
  endif

  if filereadable(s:pick_tmpfile)
    let lines = readfile(s:pick_tmpfile)
    if !empty(lines) && lines[0] =~ '^\d\{4}-\d\{2}-\d\{2}'
      " day 스크립트 출력 형식: "2025-11-27" 또는 "2025-11-27 Thu"
      " --no-weekday로 제어되므로 출력 그대로 사용
      let result = substitute(lines[0], '\s*$', '', '')  " trailing whitespace 제거
      call s:pick_callback(result)
    endif
    call delete(s:pick_tmpfile)
  endif

  let s:pick_callback = v:null
  let s:pick_tmpfile = ''
  let s:pick_include_weekday = 0

  if s:return_to_insert
    startinsert
    let s:return_to_insert = 0
  endif
endfunction

" 커서 위치에서 날짜 찾기
" 반환: [date, col_start, col_end, has_weekday] 또는 ['', 0, 0, 0]
function! s:FindDateAtCursor() abort
  let line = getline('.')
  let col = col('.')
  let date_pattern = '\d\{4}-\d\{2}-\d\{2}'
  let weekday_pattern = '\s\+\a\{3}'

  let start = 0
  while 1
    let ms = match(line, date_pattern, start)
    if ms == -1 | break | endif
    let date_matched = matchstr(line, date_pattern, start)
    let date_end = ms + len(date_matched)

    " 날짜 뒤에 요일이 있는지 명시적으로 체크
    let rest = line[date_end:]
    let weekday_match = matchstr(rest, '^' . weekday_pattern)
    let has_weekday = !empty(weekday_match)
    let me = date_end + len(weekday_match)

    " 커서가 매치 범위 내에 있는지 확인 (1-indexed col)
    if col >= ms + 1 && col <= me
      return [date_matched, ms + 1, me, has_weekday]
    endif

    let start = me
  endwhile

  return ['', 0, 0, 0]
endfunction

" 라인 내 특정 범위를 새 날짜로 교체
function! s:ReplaceDateInLine(col_start, col_end, new_date) abort
  let line = getline('.')
  let before = a:col_start > 1 ? line[:a:col_start - 2] : ''
  let after = line[a:col_end:]
  call setline('.', before . a:new_date . after)
endfunction

" 커서 위치 날짜 편집/삽입 통합
" bang: 요일 포함 여부 토글
" return_to_insert: 완료 후 insert mode 복귀
function! PickDateAtCursor(bang, ...) abort
  let s:return_to_insert = a:0 >= 1 ? a:1 : 0
  let [date, col_start, col_end, has_weekday] = s:FindDateAtCursor()

  if empty(date)
    " 삽입: bang이면 날짜만, 아니면 요일 포함
    let include_weekday = !a:bang
    call PickDate('', { d ->
      \ execute("normal! a" . d)
      \ }, include_weekday)
  else
    " 편집: bang이면 기본값 반전
    let include_weekday = a:bang ? !has_weekday : has_weekday
    call PickDate(date, { d ->
      \ s:ReplaceDateInLine(col_start, col_end, d)
      \ }, include_weekday)
  endif
endfunction

command! -bang -nargs=? PickDateAtCursor call PickDateAtCursor(<bang>0, <args>)

" 날짜 증감 (day -e 사용)
function! s:ShiftDate(date, unit, direction, has_weekday) abort
  let suffix = {'day': 'd', 'month': 'm', 'year': 'y'}[a:unit]
  let offset = (a:direction > 0 ? '+' : '') . a:direction . suffix
  let weekday_flag = a:has_weekday ? '-w' : '--no-weekday'
  let cmd = printf('%s/dots/bin/day -e %s %s %s', $HOME, a:date, offset, weekday_flag)
  return trim(system(cmd))
endfunction

" 커서 위치 날짜 증감
" direction: 1 (증가) 또는 -1 (감소)
" unit: 'year', 'month', 'day' (생략시 커서 위치로 판단)
function! ShiftDateAtCursor(direction, ...) abort
  let [date, col_start, col_end, has_weekday] = s:FindDateAtCursor()
  if empty(date) | return | endif

  if a:0 >= 1
    let unit = a:1
  else
    " 커서가 날짜의 어느 부분에 있는지 판단
    let rel_col = col('.') - col_start  " 날짜 내 상대 위치 (0-indexed)
    " YYYY-MM-DD (0-3: 년, 5-6: 월, 8-9: 일)
    if rel_col <= 3
      let unit = 'year'
    elseif rel_col <= 6
      let unit = 'month'
    else
      let unit = 'day'
    endif
  endif

  let result = s:ShiftDate(date, unit, a:direction, has_weekday)
  call s:ReplaceDateInLine(col_start, col_end, result)
endfunction

command! -nargs=+ ShiftDateAtCursor call ShiftDateAtCursor(<args>)
