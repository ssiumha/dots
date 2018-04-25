" Color list
let s:colors = [
\ "#31353c", "#b44b4b", "#5c9e5c", "#ce955b",
\ "#3879b7", "#905ddb", "#1cbfbf", "#737680",
\ "#505059", "#fe93be", "#b0ea77", "#dbdb70",
\ "#95acda", "#d47fd4", "#7ec4a0", "#989ca7",
\ "#e1e1e1", "#1d1f21", "#e1e1e1"
\ ]

func! s:H(group, fg, bg, attr)
    let fg = (a:fg is s:NUL || a:fg > 15) ? "NONE" : a:fg
    let bg = (a:bg is s:NUL || a:bg > 15) ? "NONE" : a:bg
    let guifg = (a:fg is s:NUL) ? "NONE" : s:colors[a:fg]
    let guibg = (a:bg is s:NUL) ? "NONE" : s:colors[a:bg]
    let attr = (a:attr is s:NUL) ? "NONE" : a:attr

    exec "hi ".a:group
        \ . " guifg=".guifg . " ctermfg=".fg
        \ . " guibg=".guibg . " ctermbg=".bg
        \ . " gui=".attr . " cterm=".attr
endfunc
func! s:L(group, dst_group)
    exec "hi link ".a:group." ".a:dst_group
endfunc

set background=dark
hi clear
syntax reset

let g:colors_name = "horizon"

let [s:BK1, s:RD1, s:GN1, s:YL1] = [0, 1, 2, 3]
let [s:BL1, s:VT1, s:CN1, s:GY1] = [4, 5, 6, 7]
let [s:BK2, s:RD2, s:GN2, s:YL2] = [8, 9, 10, 11]
let [s:BL2, s:VT2, s:CN2, s:GY2] = [12, 13, 14, 15]
let [s:_FG, s:_BG, s:_CU] = [16, 17, 18]
let [s:NUL, s:REV] = ["", "reverse"]


call s:H("Normal",      s:_FG, s:_BG, s:NUL)
call s:H("Cursor",      s:_CU, s:NUL, s:REV)


" Specific Text {{{
call s:H("ModeMsg",     s:BK2, s:NUL, s:NUL)
call s:H("NonText",     s:BK2, s:NUL, s:NUL)
call s:H("SpecialKey",  s:BK2, s:NUL, s:NUL)

call s:H("MoreMsg",     s:GY1, s:NUL, s:NUL)
call s:H("Question",    s:RD1, s:NUL, s:NUL)
call s:H("WarningMsg",  s:YL1, s:NUL, s:NUL)
call s:H("ErrorMsg",    s:RD1, s:NUL, s:NUL)
"}}}


" UI Syntax {{{
call s:H("Title",           s:GY2, s:NUL, s:NUL)
call s:H("Directory",       s:VT2, s:NUL, s:NUL)

call s:H("LineNr",          s:GY1, s:NUL, s:NUL)
call s:H("VertSplit",       s:BK2, s:NUL, s:NUL)
call s:H("StatusLine",      s:NUL, s:GY1, s:NUL)
call s:H("StatusLineNC",    s:NUL, s:NUL, s:NUL)
call s:H("Folded",          s:YL1, s:NUL, s:NUL)
call s:H("FoldColumn",      s:GY2, s:BK1, s:NUL)

call s:H("Visual",          s:GY1, s:NUL, s:REV)
call s:H("IncSearch",       s:GN2, s:BK2, s:NUL)
call s:H("Search",          s:NUL, s:BK2, s:NUL)

call s:H("WildMenu",        s:NUL, s:NUL, s:REV)

call s:H("TabLine",         s:NUL, s:GY1, s:NUL)
call s:H("TabLineFill",     s:NUL, s:BK2, s:NUL)
call s:H("MatchParen",      s:NUL, s:VT2, s:NUL)

if version >= 700
    call s:H("CursorLine",      s:NUL, s:BK2, s:NUL)
    call s:H("CursorColumn",    s:NUL, s:BK2, s:NUL)

    call s:H("PMenu",           s:NUL, s:VT1, s:NUL)
    call s:H("PMenuSel",        s:NUL, s:VT1, s:REV)
    call s:H("SignColumn",      s:BK2, s:NUL, s:NUL)
end
if version >= 703
    call s:H("ColorColumn",     s:NUL, s:BK2, s:NUL)
end
"}}}


" Diff {{{
call s:H("DiffAdd",         s:NUL, s:GN1, s:NUL)
call s:H("DiffChange",      s:NUL, s:YL1, s:NUL)
call s:H("DiffDelete",      s:NUL, s:RD1, s:NUL)
call s:H("DiffText",        s:VT1, s:NUL, s:NUL)

" Git COMMIT_EDITMSG
call s:H("diffAdded",       s:GN1, s:NUL, s:NUL)
call s:H("diffRemoved",     s:RD1, s:NUL, s:NUL)
" }}}


" Code Groups {{{
call s:H("Comment",         s:GY1, s:NUL, s:NUL)

call s:H("Constant",        s:YL1, s:NUL, s:NUL)
call s:H("Number",          s:YL2, s:NUL, s:NUL)
call s:H("Boolean",         s:CN1, s:NUL, s:NUL)

call s:H("Statement",       s:BL1, s:NUL, s:NUL)

call s:H("Type",            s:BL1, s:NUL, s:NUL)

call s:H("Identifier",      s:CN1, s:NUL, s:NUL)

call s:H("PreProc",         s:CN1, s:NUL, s:NUL)

call s:H("Special",         s:CN1, s:NUL, s:NUL)

"hi Underlined
call s:H("Ignore",          s:GY1, s:NUL, s:NUL)

call s:H("Error",           s:NUL, s:RD1, s:NUL)
call s:H("Todo",            s:BK1, s:YL2, s:NUL)
" }}}

delfunc s:H
delfunc s:L

