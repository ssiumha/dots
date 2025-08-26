let g:neovide_transparency = 0.97
let g:neovide_scale_factor = 1.4

" set guifont=Menlo:h14
set linespace=4
set nocursorcolumn
color spacemacs-theme

nnoremap <D-v> "+p
inoremap <D-v> <c-r>+
tnoremap <D-v> <c-r>+
cnoremap <D-v> <c-r>+

let s:session_path = '~/.local/vim/neovide_session.vim'
let s:default_dir = '~/org'
let s:default_file = 'index.md'

function! RestoreOrDefault()
  execute 'cd ' . s:default_dir
  if filereadable(expand(s:session_path))
    execute 'source ' . s:session_path
  else
    " execute 'edit ' . s:default_file
  endif

  bufdo filetype detect
endfunction

autocmd VimLeave * mksession! ~/.local/vim/neovide_session.vim
autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | call RestoreOrDefault() | endif

" call FloatermCmd(g:sh_insert_link, { out -> nvim_put(out, 'c', v:true, v:true) })
let g:sh_insert_link =<< trim EOF
link=$(fzf --prompt='LINK> ' --bind 'enter:become:[ -z {} ] && echo {q} || echo {}')
[ -z $link ] && exit 1
title=$(echo $link | fzf --prompt='TITLE> ' --bind 'enter:become:echo {q}')
echo "[$title]($link)"
EOF

" call FloatermCmd(g:open_or_create_resources, { out -> append(line('.'), out) })
let g:open_or_create_resources =<< trim EOF
doc=$(fd . --base-directory ~/org/resources -t f \
  | fzf --bind 'enter:become:[ -z {} ] && echo {q} || echo {}')
[ -z $doc ] && exit 1
echo "~/org/resources/$doc"
EOF

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
  if empty(ids)
    return 0
  endif
  for id in ids
    let name = synIDattr(id, 'name')
    if name == 'markdownCodeBlock'
      return 1
    endif
  endfor
  return 0
endfunction

function! MarkdownFoldExpr()
  if InMarkdownCodeSyntax(v:lnum)
    return '='
  endif
  let l = getline(v:lnum)
  if l =~ '^#\+\s'
    return '>' . strlen(matchstr(l, '^#\+'))
  endif
  return '='
endfunction

function! GetMarkdownPagePath()
  if synIDattr(synID(line('.'), col('.'), 1), 'name') ==# 'markdownWikiLink'
    let page = substitute(expand('<cWORD>'), '^\[\[\([^]|]\+\).*$', '\1', '')
    let page .= page =~ '\.md$' ? '' : '.md'
    let page = page =~# '^\v(/|\~|\w+:)' ? page : expand('%:p:h') . '/' . page
    return page
  else
    return ''
  endif
endfunction

function! OpenWiki() abort
  let page = GetMarkdownPagePath()
  if page != ''
    execute 'tabe' fnameescape(page)
  else
    normal! <CR>
  endif
endfunction

augroup MyMarkdown
  autocmd!
  autocmd FileType markdown setlocal foldmethod=expr foldexpr=MarkdownFoldExpr() foldenable
  autocmd FileType markdown setlocal foldtext=getline(v:foldstart).'...' foldlevel=0
  autocmd FileType markdown nnoremap <buffer> <tab> za
  autocmd FileType markdown nnoremap <buffer> <cr> :call OpenWiki()<cr>
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
    hi def MyTodo guifg=#000000 guibg=#FFD700 gui=bold
    hi def MyDone guifg=#000000 guibg=#98FB98 gui=bold
    hi def link markdownWikiLink markdownLinkText

  }

  autocmd Syntax markdown {
    syntax match MyTodo /\<TODO\>/
    syntax match MyDone /\<DONE\>/
    syntax match markdownWikiLink /\[\[.*\]\]/
    syntax match markdownDone /\v^\s*-\s\[x\]\s.*$/

    syntax match iso8601Full /\v<\d{4}-\d{2}-\d{2}T\d{2}:\d{2}(:\d{2})?([+-]\d{2}:\d{2}|Z)?>/
          \ contains=iso8601Date,iso8601T,iso8601Time,iso8601Zone
    syntax match iso8601Date /\v\d{4}-\d{2}-\d{2}/ contained
    syntax match iso8601T /T/ contained
    syntax match iso8601Time /\v\d{2}:\d{2}(:\d{2})?/ contained
    syntax match iso8601Zone /\v([+-]\d{2}:\d{2}|Z)/ contained
    hi iso8601Full cterm=NONE gui=NONE ctermbg=236 guibg=#303030
    hi link iso8601Date Number
    hi iso8601T     cterm=bold gui=bold ctermfg=Red    guifg=#ff5555 ctermbg=NONE guibg=NONE
    hi link iso8601Time Identifier
    hi link iso8601Zone Statement

  }
augroup END
