let s:colors = [
\ "#31353c", "#b44b4b", "#5c9e5c", "#ce955b",
\ "#3879b7", "#905ddb", "#1cbfbf", "#737680",
\ "#505059", "#fe93be", "#b0ea77", "#dbdb70",
\ "#95acda", "#d47fd4", "#7ec4a0", "#989ca7",
\ "#e1e1e1", "#1d1f21", "#e1e1e1"
\ ]

func! s:C(idx)
    return (a:idx > 15) ? ["NONE", "NONE"] : [s:colors[a:idx], a:idx]
endfunc

let s:p = {'normal': {}, 'inactive': {}, 'insert': {}, 'replace': {}, 'visual': {}, 'tabline': {}}
let s:p.normal.left = [ [ s:C(16), s:C(2) ], [ s:C(16), s:C(8) ] ]
let s:p.normal.right = [ [ s:C(0), s:C(15) ], [ s:C(15), s:C(8) ] ]
let s:p.inactive.left =  [ [ s:C(7), s:C(8) ], [ s:C(7), s:C(8) ] ]
let s:p.inactive.right = [ [ s:C(0), s:C(7) ], [ s:C(7), s:C(8) ] ]
let s:p.insert.left = [ [ s:C(16), s:C(4), 'reverse' ], [ s:C(4), s:C(12) ] ]
let s:p.insert.right = [ [ s:C(0), s:C(12) ], [ s:C(7), s:C(12) ] ]
let s:p.replace.left = [ [ s:C(16), s:C(1) ], s:p.normal.left[1] ]
let s:p.replace.right = copy(s:p.normal.right)
let s:p.visual.left = [ [ s:C(1), s:C(3) ], s:p.normal.left[1] ]

let s:p.normal.middle = [ [ s:C(7), s:C(0) ] ]
let s:p.inactive.middle = [ [ s:C(0), s:C(0) ] ]
let s:p.insert.middle = [ [ s:C(16), s:C(4) ] ]
let s:p.replace.middle = [ [ s:C(7), s:C(0) ] ]

let s:p.tabline.left = [ [ s:C(15), s:C(8) ] ]
let s:p.tabline.tabsel = [ [ s:C(16), s:C(17) ] ]
let s:p.tabline.middle = [ [ s:C(15), s:C(15) ] ]
let s:p.tabline.right = copy(s:p.normal.right)

let s:p.normal.error = [ [ s:C(1), s:C(0) ] ]
let s:p.normal.warning = [ [ s:C(3), s:C(0) ] ]

let g:lightline#colorscheme#horizon#palette = lightline#colorscheme#flatten(s:p)
