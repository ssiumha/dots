setlocal foldmethod=expr foldexpr=GetMarkdownFold(v:lnum)

func! GetMarkdownFold(lnum)
    let line = getline(a:lnum)
    let indent = len(matchstr(line, '\v^[#]+'))

    if indent > 0
        for id in synstack(a:lnum, 1)
            if synIDattr(id, 'name') == 'VimwikiHeaderChar'
                return '>'.indent
            endif
        endfor
    endif

    return '='
endfunc

nnoremap <silent> <buffer> <space>ml :call <sid>open_markdown_list()<cr>

func! s:open_markdown_list()
    exe 'vne | vert res32 | r!ack "^\#" #'
    exe 'norm ggdd'
    exe 'nn <buffer> <cr> 0y$<c-w>h/^<c-r>"$<cr>:noh<cr>zMzvzt'
    exe 'nn <buffer> q ZQ'
    setl nomodified
endfunc

"au BufWriteCmd *.md :Gcommit -m "[wiki] update" %
