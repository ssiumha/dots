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
  let server_name = get(s:, 'current_server', '')
  if !empty(server_name)
    setlocal statusline=%f\ [WebDAV:\ %{get(s:,'current_server','')}]%=%y\ %l,%c\ %P
  else
    setlocal statusline=%f\ [WebDAV]%=%y\ %l,%c\ %P
  endif
endif

let b:undo_ftplugin = "setlocal noswapfile< statusline<"
