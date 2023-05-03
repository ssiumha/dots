syn match MyColorVarNUL "s:NUL" containedin=vimOperParen,vimVar
syn match MyColorVarREV "s:REV" containedin=vimOperParen,vimVar
syn match MyColorVarBK1 "s:BK1" containedin=vimOperParen,vimVar
syn match MyColorVarRD1 "s:RD1" containedin=vimOperParen,vimVar
syn match MyColorVarGN1 "s:GN1" containedin=vimOperParen,vimVar
syn match MyColorVarYL1 "s:YL1" containedin=vimOperParen,vimVar
syn match MyColorVarBL1 "s:BL1" containedin=vimOperParen,vimVar
syn match MyColorVarVT1 "s:VT1" containedin=vimOperParen,vimVar
syn match MyColorVarCN1 "s:CN1" containedin=vimOperParen,vimVar
syn match MyColorVarGY1 "s:GY1" containedin=vimOperParen,vimVar

syn match MyColorVarBK2 "s:BK2" containedin=vimOperParen,vimVar
syn match MyColorVarRD2 "s:RD2" containedin=vimOperParen,vimVar
syn match MyColorVarGN2 "s:GN2" containedin=vimOperParen,vimVar
syn match MyColorVarYL2 "s:YL2" containedin=vimOperParen,vimVar
syn match MyColorVarBL2 "s:BL2" containedin=vimOperParen,vimVar
syn match MyColorVarVT2 "s:VT2" containedin=vimOperParen,vimVar
syn match MyColorVarCN2 "s:CN2" containedin=vimOperParen,vimVar
syn match MyColorVarGY2 "s:GY2" containedin=vimOperParen,vimVar

syn match MyColorVar_FG "s:_FG" containedin=vimOperParen,vimVar
syn match MyColorVar_BG "s:_BG" containedin=vimOperParen,vimVar
syn match MyColorVar_CU "s:_CU" containedin=vimOperParen,vimVar
syn match MyColorVarNUL "s:NUL" containedin=vimOperParen,vimVar
syn match MyColorVarREV "s:REV" containedin=vimOperParen,vimVar


syn match MyColorVarBK1_BG /, \zss:BK1\ze, / containedin=vimOperParen,vimVar
syn match MyColorVarRD1_BG /, \zss:RD1\ze, / containedin=vimOperParen,vimVar
syn match MyColorVarGN1_BG /, \zss:GN1\ze, / containedin=vimOperParen,vimVar
syn match MyColorVarYL1_BG /, \zss:YL1\ze, / containedin=vimOperParen,vimVar
syn match MyColorVarBL1_BG /, \zss:BL1\ze, / containedin=vimOperParen,vimVar
syn match MyColorVarVT1_BG /, \zss:VT1\ze, / containedin=vimOperParen,vimVar
syn match MyColorVarCN1_BG /, \zss:CN1\ze, / containedin=vimOperParen,vimVar
syn match MyColorVarGY1_BG /, \zss:GY1\ze, / containedin=vimOperParen,vimVar

syn match MyColorVarBK2_BG /, \zss:BK2\ze, / containedin=vimOperParen,vimVar
syn match MyColorVarRD2_BG /, \zss:RD2\ze, / containedin=vimOperParen,vimVar
syn match MyColorVarGN2_BG /, \zss:GN2\ze, / containedin=vimOperParen,vimVar
syn match MyColorVarYL2_BG /, \zss:YL2\ze, / containedin=vimOperParen,vimVar
syn match MyColorVarBL2_BG /, \zss:BL2\ze, / containedin=vimOperParen,vimVar
syn match MyColorVarVT2_BG /, \zss:VT2\ze, / containedin=vimOperParen,vimVar
syn match MyColorVarCN2_BG /, \zss:CN2\ze, / containedin=vimOperParen,vimVar
syn match MyColorVarGY2_BG /, \zss:GY2\ze, / containedin=vimOperParen,vimVar



hi def MyColorVarBK1 ctermfg=0
hi def MyColorVarRD1 ctermfg=1
hi def MyColorVarGN1 ctermfg=2
hi def MyColorVarYL1 ctermfg=3
hi def MyColorVarBL1 ctermfg=4
hi def MyColorVarVT1 ctermfg=5
hi def MyColorVarCN1 ctermfg=6
hi def MyColorVarGY1 ctermfg=7

hi def MyColorVarBK2 ctermfg=8
hi def MyColorVarRD2 ctermfg=9
hi def MyColorVarGN2 ctermfg=10
hi def MyColorVarYL2 ctermfg=11
hi def MyColorVarBL2 ctermfg=12
hi def MyColorVarVT2 ctermfg=13
hi def MyColorVarCN2 ctermfg=14
hi def MyColorVarGY2 ctermfg=15

hi def MyColorVar_FG ctermfg=NONE
hi def MyColorVar_BG ctermfg=NONE
hi def MyColorVar_CU ctermfg=NONE
hi def MyColorVarNUL ctermfg=NONE
hi def MyColorVarREV cterm=reverse ctermfg=NONE

hi def MyColorVarBK1_BG cterm=reverse ctermfg=0
hi def MyColorVarRD1_BG cterm=reverse ctermfg=1
hi def MyColorVarGN1_BG cterm=reverse ctermfg=2
hi def MyColorVarYL1_BG cterm=reverse ctermfg=3
hi def MyColorVarBL1_BG cterm=reverse ctermfg=4
hi def MyColorVarVT1_BG cterm=reverse ctermfg=5
hi def MyColorVarCN1_BG cterm=reverse ctermfg=6
hi def MyColorVarGY1_BG cterm=reverse ctermfg=7

hi def MyColorVarBK2_BG cterm=reverse ctermfg=8
hi def MyColorVarRD2_BG cterm=reverse ctermfg=9
hi def MyColorVarGN2_BG cterm=reverse ctermfg=10
hi def MyColorVarYL2_BG cterm=reverse ctermfg=11
hi def MyColorVarBL2_BG cterm=reverse ctermfg=12
hi def MyColorVarVT2_BG cterm=reverse ctermfg=13
hi def MyColorVarCN2_BG cterm=reverse ctermfg=14
hi def MyColorVarGY2_BG cterm=reverse ctermfg=15
