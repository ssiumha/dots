if exists("b:current_syntax")
  echo b:current_syntax
  finish
endif

syn clear
syn sync fromstart

syn match adocTitle /\v^\={1,5}\s+.+$/
syn region adocDelimitedBlock start=/\v^-{4,}/ end=/^\v-{4,}/
syn match adocList /\v^\s*\zs(-|*{1,5}|\.{1,5})\ze\s/

hi def link adocTitle Title
hi def link adocDelimitedBlock Statement
hi def link adocList Label

let b:current_syntax = "asciidoc"
