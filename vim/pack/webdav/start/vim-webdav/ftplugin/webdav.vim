" Vim filetype plugin file
" Language: WebDAV File Buffer
" Maintainer: vim-webdav
" Last Change: 2025

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

" Buffer-local settings for WebDAV-managed files
setlocal noswapfile

" Show WebDAV info in status line
if has('statusline')
  let server_name = get(b:, 'webdav_server', '')
  if !empty(server_name)
    setlocal statusline=%f\ [WebDAV:\ %{get(b:,'webdav_server','')}]%=%y\ %l,%c\ %P
  else
    setlocal statusline=%f\ [WebDAV]%=%y\ %l,%c\ %P
  endif
endif

" Key mapping: - to open parent directory listing
nnoremap <buffer> <silent> - :WebDAVList<CR>

" Key mappings for wikilink navigation
" gf: follow wikilink under cursor (like vim's gf for goto file)
nnoremap <buffer> <silent> gf :call webdav#wikilink#open()<CR>
" Enter: also follow wikilink (convenient for quick navigation)
nnoremap <buffer> <silent> <CR> :call webdav#wikilink#open()<CR>

let b:undo_ftplugin = "setlocal noswapfile< statusline< | silent! nunmap <buffer> - | silent! nunmap <buffer> gf | silent! nunmap <buffer> <CR>"
