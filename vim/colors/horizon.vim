" Color list
let s:colors = [
\ "#31353c", "#b44b4b", "#5c9e5c", "#ce955b",
\ "#3879b7", "#905ddb", "#1cbfbf", "#737680",
\ "#505059", "#fe93be", "#b0ea77", "#dbdb70",
\ "#95acda", "#d47fd4", "#7ec4a0", "#989ca7",
\ "#e1e1e1", "#1d1f21", "#e1e1e1"
\ ]

func! s:H(group, fg, bg, attr)
    let fg = (a:fg is "" || a:fg > 15) ? "NONE" : a:fg
    let bg = (a:bg is "" || a:bg > 15) ? "NONE" : a:bg
    let guifg = (a:fg is "") ? "NONE" : s:colors[a:fg]
    let guibg = (a:bg is "") ? "NONE" : s:colors[a:bg]
    let attr = (a:attr is "") ? "NONE" : a:attr

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

call s:H("Normal", "16", "17", "")
call s:H("Cursor", "18", "", "reverse")


" Specific Text {{{
call s:H("ModeMsg", 8, "", "")
call s:H("NonText", 8, "", "")
call s:H("SpecialKey", 8, "", "")

call s:H("MoreMsg", 7, "", "")
call s:H("Question", 1, "", "")
call s:H("WarningMsg", 3, "", "")
call s:H("ErrorMsg", 1, "", "")
"}}}


" UI Syntax {{{
call s:H("Title", 15, "", "")
call s:H("Directory", 13, "", "")

call s:H("LineNr", 7, "", "")
call s:H("VertSplit", 8, "", "")
call s:H("StatusLine", "", 7, "")
call s:H("StatusLineNC", "", "", "")
call s:H("Folded", 3, "", "")
call s:H("FoldColumn", 15, 0, "")

call s:H("Visual", 7, "", "reverse")
call s:H("IncSearch", 10, 2, "")
call s:H("Search", "", 2, "")

call s:H("WildMenu", "", "", "reverse")

call s:H("TabLine", "", 7, "")
call s:H("TabLineFill", "", 8, "")
call s:H("MatchParen", "", 13, "")

if version >= 700
    call s:H("CursorLine", "", 8, "")
    call s:H("CursorColumn", "", 8, "")

    call s:H("PMenu", "", 5, "")
    call s:H("PMenuSel", "", 5, "reverse")
    call s:H("SignColumn", 8, "", "")
end
if version >= 703
    call s:H("ColorColumn", "", 8, "")
end
"}}}


" Diff {{{
call s:H("DiffAdd", "", 2, "")
call s:H("DiffChange", "", 3, "")
call s:H("DiffDelete", "", 1, "")
call s:H("DiffText", 5, "", "")

" Git COMMIT_EDITMSG
call s:H("diffAdded", 2, "", "")
call s:H("diffRemoved", 1, "", "")
" }}}


" Code Groups {{{
call s:H("Comment", 7, "", "")

call s:H("Constant", 3, "", "")
call s:H("Number", 11, "", "")
call s:H("Boolean", 6, "", "")

call s:H("Statement", 4, "", "")

call s:H("Type", 4, "", "")

call s:H("Identifier", 6, "", "")

call s:H("PreProc", 6, "", "")

call s:H("Special", 6, "", "")

"hi Underlined
call s:H("Ignore", 7, "", "")

call s:H("Error", "", 1, "")
call s:H("Todo", "", 11, "")
" }}}

delfunc s:H
delfunc s:L

