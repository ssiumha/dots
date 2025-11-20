if exists('g:loaded_webdav')
  finish
endif
let g:loaded_webdav = 1

" Preview settings
let g:webdav_preview_enabled = get(g:, 'webdav_preview_enabled', 1)
let g:webdav_preview_lines = get(g:, 'webdav_preview_lines', 200)
let g:webdav_preview_use_bat = get(g:, 'webdav_preview_use_bat', 1)

" Constants for HTTP response parsing

" Helper function to get server name from buffer or default URL
" Returns server_name string, or v:null if validation failed


" Parse WebDAV URL with authentication
" Input: https://user:pass@host:port/path or http://host/path
" Returns: {'url': 'https://host:port/path', 'user': 'user', 'pass': 'pass'}

" Scan environment variables for WEBDAV_UI_* pattern
" Returns: Dictionary of {name: {url: 'url', user: 'user', 'pass': 'pass'}, ...}

" Get server info by server name
" Returns: Dictionary with 'url', 'user', 'pass' keys
" Priority: WEBDAV_UI_{NAME} -> WEBDAV_UI_DEFAULT -> WEBDAV_DEFAULT_* -> error

" Public wrapper for webdav#file#get (called from autoload/webdav/fzf.vim)
function! WebDAVGet(path, server_name = '')
  call webdav#file#get(a:path, a:server_name)
endfunction

" Public wrapper for webdav#ui#list (called from autoload/webdav/operations.vim)
function! WebDAVList(path, server_name = '')
  call webdav#ui#list(a:path, a:server_name)
endfunction

command! -nargs=? WebDAVList call webdav#ui#list(<q-args>)
command! -nargs=1 WebDAVGet call webdav#file#get(<q-args>)
command! WebDAVPut call webdav#file#put()
command! WebDAVDiff call webdav#conflict#diff()
command! -nargs=? WebDAVUI call webdav#ui#main(<q-args>)
command! WebDAVUIFzf call webdav#ui#main_fzf()
command! -bang -nargs=* WebDAVFzf call webdav#fzf#main([<f-args>], <bang>0)
command! WebDAVRecent call webdav#recent#list()
command! WebDAVRecentFzf call webdav#recent#fzf()
command! -nargs=+ WebDAVNote call webdav#note#open(<f-args>)

" Setup autocmd for WebDAV buffers (ONLY for webdav:// protocol buffers)
augroup webdav_buffers
  autocmd!
  " Handle :e! (force reload) for WebDAV buffers only
  " This ONLY affects buffers with webdav:// protocol - normal files are unaffected
  autocmd BufReadCmd webdav://* call webdav#buffer#reload()
  " Note: BufWriteCmd is set per-buffer in webdav#buffer#setup()
augroup END

" Test helpers - expose script-local functions for testing
if exists('$WEBDAV_TEST_MODE') && $WEBDAV_TEST_MODE == '1'
  function! TestWebDAVRecursiveScan(path)
    return webdav#fzf#scan_recursive(a:path)
  endfunction

  function! TestResolveServerAndPath(args)
    return webdav#fzf#resolve_args(a:args)
  endfunction

  function! TestGetScanCache()
    return webdav#cache#get_data()
  endfunction

  function! TestWebDAVFzfOpen(base_path, selection)
    return s:WebDAVFzfOpen(a:base_path, a:selection)
  endfunction

  function! TestGetRecentFiles()
    return webdav#recent#get_files()
  endfunction

  function! TestLoadRecentFiles()
    call webdav#recent#load()
  endfunction

  function! TestSaveRecentFiles()
    call webdav#recent#save()
  endfunction

  function! TestTrackRecentFile(path, url, server)
    call webdav#recent#track(a:path, a:url, a:server)
  endfunction

  function! TestWebDAVRecentFzfSink(selection)
    call webdav#recent#fzf_sink(a:selection)
  endfunction
endif

" Initialize plugin - load recent files from cache
call webdav#recent#load()
