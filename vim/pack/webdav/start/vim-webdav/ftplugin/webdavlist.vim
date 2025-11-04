" Vim filetype plugin file
" Language: WebDAV Directory Listing
" Maintainer: vim-webdav
" Last Change: 2025

if exists("b:did_ftplugin")
  finish
endif
let b:did_ftplugin = 1

" Buffer-local settings
setlocal buftype=nofile
setlocal bufhidden=wipe
setlocal noswapfile
setlocal nomodifiable
setlocal nowrap
setlocal cursorline

" Conceal settings (optional - hide some visual clutter)
setlocal conceallevel=0

" Status line
if has('statusline')
  setlocal statusline=%{get(b:,'webdav_current_path','')}%=WebDAV\ List
endif

let b:undo_ftplugin = "setlocal buftype< bufhidden< noswapfile< nomodifiable< nowrap< cursorline< conceallevel< statusline<"
