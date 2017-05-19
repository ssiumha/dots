setl foldmethod=expr

func! AdocFoldFunc(lnum)
    let line = getline(a:lnum)
    if len(line) > 0
        let indent = len(matchstr(line, '\v^[=]+'))
        retu indent ? '>'.indent : '='
    en

    let l = a:lnum + 1
    let max = line('$')
    wh l <= max
        let line = getline(l)
        let indent = len(matchstr(line, '\v^[=]+'))
        if  indent == 0 && len(line) > 0
            retu '-1'
        elsei indent > 0
            retu indent - 1
        en
        let l = l + 1
    endw

    retu '='
endf
setl foldexpr=AdocFoldFunc(v:lnum)

func! s:make_open_link_cmd()
    let line = getline('.')
    let pattern = 'link:[^[]*\[[^]]*]'
    let linkpos = match(line, pattern)
    let colpos = col('.')

    if linkpos == -1
        return ''
    endif

    while 1
        let nextpos = match(line, pattern, linkpos + 1)
        if nextpos == -1 || nextpos >= colpos
            break
        endif
        let linkpos = nextpos
    endwhile

    let m = matchstr(line, pattern, linkpos)
    if colpos <= linkpos || linkpos + len(m) < colpos
        return ''
    endif

    let filename = substitute(m, '\vlink:(.*)\[.*', '\1', '')
    return 'edit '.expand('%:p:h').'/'.filename
endfunc
nnoremap <buffer> <silent> <cr> :execute <sid>make_open_link_cmd()<cr>

