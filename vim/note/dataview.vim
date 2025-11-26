function! s:ExtractDataviewFields()
  let line = getline('.')
  let fields = []
  let start = 0
  while 1
    let ms = match(line, '\[', start)
    if ms == -1 | break | endif
    let me = match(line, '\]', ms)
    if me == -1 | break | endif
    let content = line[ms+1 : me-1]
    if content =~ '::'
      let [key, val] = split(content, '::\s*', 1)
      call add(fields, {'key': key, 'val': val, 'start': ms, 'end': me+1})
    endif
    let start = me + 1
  endwhile
  return fields
endfunction

function! s:ApplyDataviewFields(fields)
  let line = getline('.')
  let line = substitute(line, '\s*\[[^\[\]]*::[^\[\]]*\]', '', 'g')
  for f in a:fields
    let line .= ' [' . f.key . ':: ' . f.val . ']'
  endfor
  call setline('.', line)
endfunction

function! DataviewProperty() abort
  let fields = s:ExtractDataviewFields()
  let items = map(copy(fields), {_, f -> f.key . ':: ' . f.val})
  call add(items, '[+ 새 필드]')

  call fzf#run(fzf#wrap({
    \ 'source': items,
    \ 'options': ['--prompt=Field> ', '--expect=ctrl-d', '--header=enter:edit ctrl-d:delete'],
    \ 'sink*': {lines -> s:HandlePropertySelection(lines, fields)},
    \ }))
endfunction

function! s:HandlePropertySelection(lines, fields)
  if len(a:lines) < 2 | return | endif
  let [action, selected] = [a:lines[0], a:lines[1]]

  if selected == '[+ 새 필드]'
    let key = input('Key: ')
    if empty(key) | return | endif
    let val = input('Value: ')
    call add(a:fields, {'key': key, 'val': val})
    call s:ApplyDataviewFields(a:fields)
    return
  endif

  let idx = index(map(copy(a:fields), {_, f -> f.key . ':: ' . f.val}), selected)
  if idx == -1 | return | endif

  if action == 'ctrl-d'
    call remove(a:fields, idx)
  else
    let new_val = input(a:fields[idx].key . ': ', a:fields[idx].val)
    if empty(new_val) | return | endif
    let a:fields[idx].val = new_val
  endif
  call s:ApplyDataviewFields(a:fields)
endfunction
