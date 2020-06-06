syn match MemoNone "\[_\].*$"
syn match MemoCheck "\[V\].*$"
syn match MemoCross "\[X\].*$"
syn match MemoTitle "\*.*$" containedin=ALL
syn match MemoDate "#.*$" containedin=ALL
syn match MemoComment "//.*$" containedin=ALL

"syn match MemoListElem "-.*"
syn match MemoCheckListElem "\v(\s*)\[V\] (.*\n)(\1\t-.*\n*)+"
syn match MemoCrossListElem "\v(\s*)\[X\] (.*\n)(\1\t-.*\n*)+"

hi MemoNone guifg=gray ctermfg=gray

hi MemoCheck guifg=seagreen ctermfg=darkgreen
hi link MemoCheckListElem MemoCheck

hi MemoCross guifg=lightred
hi link MemoCrossListElem MemoCross

hi MemoTitle guifg=orange
hi MemoDate guifg=gray guibg=darkslateblue gui=italic
hi MemoComment guifg=darkgray
hi MemoListElem guifg=darkgray

"rgb, cmyk w gray
"brown
"orange dark
"seagreen dark light
"slateblue dark light
"violet dark
"purple
