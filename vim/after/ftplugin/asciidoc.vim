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
