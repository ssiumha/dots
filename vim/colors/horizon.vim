" Color list
let s:colors = [
\ "1d1f21", "cb7676", "63b463", "ce955b",
\ "6891ba", "9d81c6", "52baba", "737680",
\ "505059", "dfa1ba", "abd284", "dbdb70",
\ "95acda", "bc7fbc", "7ec4a0", "e1e1e1"
\ ]

func! <sid>H(group, fg, bg, attr)
    let fg = (a:fg is "") ? "none" : a:fg
    let bg = (a:bg is "") ? "none" : a:bg
    let attr = (a:attr is "") ? "none" : a:attr

    exec "hi ".a:group
        \ . " guifg=#".s:colors[fg] . " ctermfg=".fg
        \ . " guibg=#".s:colors[bg] . " ctermbg=".bg
        \ . " gui=".attr . " cterm=".attr
endfunc
func! <sid>L(group, dst_group)
    exec "hi link ".a:group." ".a:dst_group
endfunc

set background=dark
hi clear
syntax reset

let g:colors_name = "horizon"

call <sid>H("Normal", "", "", "")
call <sid>H("Cursor", "", "", "reverse")


" Specific Text {{{
call <sid>H("ModeMsg", 8, "", "")
call <sid>H("NonText", 8, "", "")
call <sid>H("SpecialKey", 8, "", "")

call <sid>H("MoreMsg", 7, "", "")
call <sid>H("Question", 1, "", "")
call <sid>H("WarningMsg", 3, "", "")
call <sid>H("ErrorMsg", 1, 0, "")
"}}}


" UI Syntax {{{
call <sid>H("Title", 15, "", "")
call <sid>H("Directory", 13, "", "")

call <sid>H("LineNr", 7, "", "")
call <sid>H("VertSplit", 8, "", "")
call <sid>H("StatusLine", 0, 7, "")
call <sid>H("StatusLineNC", 0, 0, "")
call <sid>H("Folded", 3, "", "")
call <sid>H("FoldColumn", 3, "", "")

call <sid>H("Visual", 7, "", "reverse")
call <sid>H("IncSearch", 10, 2, "")
call <sid>H("Search", 0, 2, "")

call <sid>H("WildMenu", "", "", "reverse")

call <sid>H("TabLine", 0, 7, "")
call <sid>H("TabLineFill", 0, 8, "")
call <sid>H("MatchParen", "", 13, "")

if version >= 700
    call <sid>H("CursorLine", "", 8, "")
    call <sid>H("CursorColumn", "", 8, "")

    call <sid>H("PMenu", 0, 5, "")
    call <sid>H("PMenuSel", 0, 5, "reverse")
    call <sid>H("SignColumn", 8, "", "")
end
if version >= 703
    call <sid>H("ColorColumn", "", 8, "")
end
"}}}


" Diff {{{
call <sid>H("DiffAdd", "", 10, "")
call <sid>H("DiffChange", "", 11, "")
call <sid>H("DiffDelete", "", 9, "")
call <sid>H("DiffText", 5, "", "")
" }}}


" Code Groups {{{
call <sid>H("Comment", 7, "", "")

call <sid>H("Constant", 3, "", "")
call <sid>H("Number", 11, "", "")
call <sid>H("Boolean", 6, "", "")

call <sid>H("Statement", 4, "", "")
call <sid>L("Operator", "Normal")

call <sid>H("Type", 4, "", "")

call <sid>H("Identifier", 6, "", "")

call <sid>H("PreProc", 6, "", "")

call <sid>H("Special", 6, "", "")

"hi Underlined
call <sid>H("Ignore", 7, "", "")

call <sid>H("Error", "", 1, "")
call <sid>H("Todo", 0, 11, "")
" }}}

delfunc <sid>H
delfunc <sid>L

