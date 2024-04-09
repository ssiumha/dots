" reference: https://github.com/SirVer/ultisnips/blob/master/syntax/snippets.vim

if exists("b:current_syntax")
  finish
endif

syn region snipSnippet start="^snippet\_s" end="^endsnippet\s*$" contains=snipSnippetHeader fold keepend
syn match snipSnippetHeader "^.*$" nextgroup=snipSnippetBody,snipSnippetFooter skipnl contained contains=snipSnippetHeaderKeyword
syn match snipSnippetHeaderKeyword "^snippet" contained nextgroup=snipSnippetTrigger skipwhite
syn region snipSnippetBody start="\_." end="^\zeendsnippet\s*$" contained nextgroup=snipSnippetFooter contains=snipLeadingSpaces,@snipTokens
syn match snipSnippetFooter "^endsnippet.*" contained contains=snipSnippetFooterKeyword
syn match snipSnippetFooterKeyword "^endsnippet" contained

syn match snipSnippetTrigger "\S\+" contained nextgroup=snipSnippetDocString,snipSnippetTriggerInvalid skipwhite
syn match snipSnippetTrigger ,".\{-}"\ze\%(\s\+"\%(\s*\S\)\@=[^"]*\%("\s\+[^"[:space:]]\+\|"\)\=\)\=\s*$, contained nextgroup=snipSnippetDocString skipwhite
syn match snipSnippetTrigger ,\%(\(\S\).\{-}\1\|\S\+\)\ze\%(\s\+"[^"]*\%("\s\+\%("[^"]\+"\s\+[^"[:space:]]*e[^"[:space:]]*\)\|"\)\=\)\=\s*$, contained nextgroup=snipSnippetDocContextString skipwhite
syn match snipSnippetTrigger ,\([^"[:space:]]\).\{-}\1\%(\s*$\)\@!\ze\%(\s\+"[^"]*\%("\s\+\%("[^"]\+"\s\+[^"[:space:]]*e[^"[:space:]]*\|[^"[:space:]]\+\)\|"\)\=\)\=\s*$, contained nextgroup=snipSnippetDocString skipwhite
syn match snipSnippetTriggerInvalid ,\S\@=.\{-}\S\ze\%(\s\+"[^"]*\%("\s\+[^"[:space:]]\+\s*\|"\s*\)\=\|\s*\)$, contained nextgroup=snipSnippetDocString skipwhite
syn match snipSnippetDocString ,"[^"]*", contained nextgroup=snipSnippetOptions skipwhite
syn match snipSnippetDocContextString ,"[^"]*", contained nextgroup=snipSnippetContext skipwhite
syn match snipSnippetContext ,"[^"]\+", contained skipwhite
syn match snipSnippetOptions ,\S\+, contained contains=snipSnippetOptionFlag
syn match snipSnippetOptionFlag ,[biwrtsmxAe], contained

hi def link snipKeyword          Keyword
hi def link snipSnippetHeaderKeyword snipKeyword
hi def link snipSnippetFooterKeyword snipKeyword

hi def link snipSnippetTrigger        Identifier
hi def link snipSnippetTriggerInvalid Error
hi def link snipSnippetDocString      String
hi def link snipSnippetDocContextString String
hi def link snipSnippetOptionFlag     Special

let b:current_syntax = "snips"
