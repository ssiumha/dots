" autoload/webdav/cache.vim - FZF scan result caching
" Functions for caching WebDAV directory scan results

" Module-local cache variable
let s:scan_cache = {}  " Cache for fzf recursive scans: {url: {path: [files]}}

" Generate cache key from server name and URL
function! webdav#cache#get_key(server_name, url)
  if empty(a:server_name)
    return '__default__@' . a:url
  endif
  return a:server_name . '@' . a:url
endfunction

" Invalidate cache for a specific file path
" Parameters: server_name (string), server_info (dict), file_path (string)
function! webdav#cache#invalidate(server_name, server_info, file_path)
  let cache_key = webdav#cache#get_key(a:server_name, a:server_info.url)

  if !has_key(s:scan_cache, cache_key)
    return
  endif

  " Get directory path (parent of file)
  let dir_path = fnamemodify(a:file_path, ':h') . '/'

  " Remove cache for this directory
  if has_key(s:scan_cache[cache_key], dir_path)
    unlet s:scan_cache[cache_key][dir_path]
    call webdav#core#debug_log("Cache invalidated: " . cache_key . " -> " . dir_path)
  endif
endfunction

" Get reference to cache data (for testing and internal use)
function! webdav#cache#get_data()
  return s:scan_cache
endfunction
