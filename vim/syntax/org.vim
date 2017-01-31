setl foldmethod=expr

fu! OrgFoldfunc(lnum)
    let line = getline(a:lnum)
    if len(line) > 0
        let indent = len(matchstr(line, '\v^[*]+'))
        retu indent ? '>'.indent : '='
    en

    let l = a:lnum + 1
    let max = line('$')
    wh l <= max
        let line = getline(l)
        let indent = len(matchstr(line, '\v^[*]+'))
        if  indent == 0 && len(line) > 0
            retu '-1'
        elsei indent > 0
            retu indent - 1
        en
        let l = l + 1
    endw

    retu '='
endf
setl foldexpr=OrgFoldfunc(v:lnum)

fu! OrgFoldText()
    let line = getline(v:foldstart)

    let nucolwidth = &fdc + &number * &numberwidth
    let windowwidth = winwidth(0) - nucolwidth - 3
    let foldedlinecount = v:foldend - v:foldstart

    let fillcharcount = windowwidth - virtcol([v:foldstart, '$']) - len(foldedlinecount)
    retu line . '…' . repeat(" ",fillcharcount) . foldedlinecount . '…' . ' '
endf
setl foldtext=OrgFoldText()

syn match OrgHeader /^\*.*$/
hi OrgHeader guifg=#abd284 ctermfg=green

ab ?o-d? <<c-r>=strftime('%Y-%m-%d ').['Sun','Mon','Tue','Wed','Thu','Fri','Sat'][strftime('%w')]<cr>>
ab ?o-sch? SCHEDULED: ?o-d?
ab ?o-dl? DEADLINE: ?o-d?
ab ?o-dt? ENDTIME: ?o-d?

"Week-agenda (W36):
"Monday         3   September 2012 W36
"Monday         3   September 2012
"Monday         3   September 2012
"Monday         3   September 2012
"   website:    Schduled: TODO Make aaaa
"Monday         3   September 2012
"   website:    In  1 d.: TODO Make aaaa
"   website:    Deadline: TODO Make bbbb
