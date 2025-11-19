" autoload/webdav/recent.vim - Recent files tracking and management
" Functions for tracking recently accessed WebDAV files

" Module-local variables
let s:recent_files = []  " Recent files list: [{path, url, server, timestamp}, ...]
let s:recent_files_path = !empty($WEBDAV_RECENT_CACHE_PATH) ? expand($WEBDAV_RECENT_CACHE_PATH) : expand('~/.cache/vim-webdav/recent.json')
let s:recent_files_max = 50  " Maximum number of recent files to keep

" Load recent files from persistent storage
function! webdav#recent#load()
  if !filereadable(s:recent_files_path)
    call webdav#core#debug_log("DEBUG RECENT: No recent files cache found")
    return
  endif

  try
    let json_data = join(readfile(s:recent_files_path), '')
    let s:recent_files = json_decode(json_data)
    call webdav#core#debug_log("DEBUG RECENT: Loaded " . len(s:recent_files) . " recent files")
  catch
    call webdav#core#debug_log("DEBUG RECENT: Failed to load recent files: " . v:exception)
    let s:recent_files = []
  endtry
endfunction

" Save recent files to persistent storage
function! webdav#recent#save()
  try
    " Ensure cache directory exists
    let cache_dir = fnamemodify(s:recent_files_path, ':h')
    if !isdirectory(cache_dir)
      call mkdir(cache_dir, 'p')
    endif

    " Write JSON to file
    let json_data = json_encode(s:recent_files)
    call writefile([json_data], s:recent_files_path)
    call webdav#core#debug_log("DEBUG RECENT: Saved " . len(s:recent_files) . " recent files")
  catch
    call webdav#core#debug_log("DEBUG RECENT: Failed to save recent files: " . v:exception)
  endtry
endfunction

" Track recently opened file
" Parameters: path, url, server
function! webdav#recent#track(path, url, server)
  " Create new entry
  let new_entry = {
    \ 'path': a:path,
    \ 'url': a:url,
    \ 'server': a:server,
    \ 'timestamp': localtime()
  \ }

  " Remove duplicates (same path and url)
  let filtered = filter(copy(s:recent_files), 'v:val.path != a:path || v:val.url != a:url')

  " Add new entry at the beginning (most recent first)
  let s:recent_files = [new_entry] + filtered

  " Limit to maximum number of entries
  if len(s:recent_files) > s:recent_files_max
    let s:recent_files = s:recent_files[0:s:recent_files_max-1]
  endif

  " Save immediately
  call webdav#recent#save()

  call webdav#core#debug_log("DEBUG RECENT: Tracked file " . a:path . " (total: " . len(s:recent_files) . ")")
endfunction

" Format timestamp as human-readable time ago
function! webdav#recent#format_time_ago(timestamp)
  let now = localtime()
  let diff = now - a:timestamp

  if diff < 60
    return 'just now'
  elseif diff < 3600
    let minutes = diff / 60
    return minutes . 'm ago'
  elseif diff < 86400
    let hours = diff / 3600
    return hours . 'h ago'
  elseif diff < 604800
    let days = diff / 86400
    return days . 'd ago'
  else
    return strftime('%Y-%m-%d', a:timestamp)
  endif
endfunction

" Open a recent file entry
function! webdav#recent#open(entry)
  if type(a:entry) != v:t_dict
    echoerr "Error: Invalid entry type"
    return
  endif

  " Validate entry has required fields
  if !has_key(a:entry, 'path') || !has_key(a:entry, 'url') || !has_key(a:entry, 'server')
    echoerr "Error: Invalid entry format"
    return
  endif

  " Check if server is still configured
  let servers = webdav#server#scan()
  let server_name = a:entry.server

  " Handle empty or default server name
  if empty(server_name) || server_name == 'default'
    " Use default environment variables (webdav#server#get_info will validate)
    " Continue with default server
  else
    " Validate server exists
    if !has_key(servers, server_name)
      echoerr "Error: Server '" . server_name . "' not found or not configured"
      echo "Available servers: " . join(sort(keys(servers)), ', ')
      return
    endif

    " Use server_name when opening the file
    let server_info = servers[server_name]
    echo "Opening from server: " . server_name
  endif

  " Open file in new tab
  tabnew
  call webdav#file#get(a:entry.path, server_name)
endfunction

" Handle selection from recent files list
function! webdav#recent#select()
  let line = getline('.')

  " Skip header and empty lines
  if empty(trim(line)) || line =~ '^"'
    return
  endif

  " Get line number (1-indexed, but skip header line)
  let line_num = line('.') - 2

  if line_num < 0 || line_num >= len(s:recent_files)
    echo "Invalid selection"
    return
  endif

  " Get the entry
  let entry = s:recent_files[line_num]

  " Open the file
  call webdav#recent#open(entry)
endfunction

" Show recent files in a list buffer
function! webdav#recent#list()
  if empty(s:recent_files)
    echo "No recent WebDAV files"
    return
  endif

  " Create new buffer
  tabnew

  " Configure buffer
  setlocal buftype=nofile bufhidden=wipe
  setlocal modifiable

  " Add header
  call setline(1, '" Recent WebDAV Files')

  " Add each recent file
  let idx = 2
  for entry in s:recent_files
    let time_ago = webdav#recent#format_time_ago(entry.timestamp)
    let server_info = empty(entry.server) ? '' : ' (' . entry.server . ')'
    let line = printf('[%s] %s%s', time_ago, entry.path, server_info)
    call setline(idx, line)
    let idx += 1
  endfor

  " Set filetype and make read-only
  setlocal filetype=webdavlist
  setlocal nomodifiable

  " Move cursor to first file
  normal! gg
  normal! j

  " Key mapping for opening files
  nnoremap <buffer> <CR> :call webdav#recent#select()<CR>
endfunction

" Handle fzf selection from recent files
function! webdav#recent#fzf_sink(selection)
  if empty(a:selection)
    return
  endif

  " Parse TSV format: [time]\tpath\tserver
  let parts = split(a:selection, "\t")
  if len(parts) < 2
    echoerr "Error: Invalid selection format"
    return
  endif

  let path = parts[1]

  " Find matching entry in recent files
  for entry in s:recent_files
    if entry.path == path
      call webdav#recent#open(entry)
      return
    endif
  endfor

  echoerr "Error: File not found in recent list: " . path
endfunction

" Show recent files in fzf
function! webdav#recent#fzf()
  if empty(s:recent_files)
    echo "No recent WebDAV files"
    return
  endif

  " Check if fzf is available
  if !executable('fzf')
    echoerr "Error: fzf is not installed"
    return
  endif

  " Format recent files for fzf (TSV format)
  let formatted = []
  for entry in s:recent_files
    let time_ago = webdav#recent#format_time_ago(entry.timestamp)
    let server = empty(entry.server) ? '' : entry.server
    " Format: [time]\tpath\tserver\turl (url is hidden but used for preview)
    let line = printf("[%s]\t%s\t%s\t%s", time_ago, entry.path, server, entry.url)
    call add(formatted, line)
  endfor

  " Build fzf options (preview disabled for recent files - mixed servers)
  let fzf_options = ['--prompt', 'Recent> ', '--delimiter', '\t', '--with-nth', '1,2,3']

  " Launch fzf (80% window size for better visibility)
  call fzf#run(fzf#wrap({
    \ 'source': formatted,
    \ 'sink': function('webdav#recent#fzf_sink'),
    \ 'options': fzf_options,
    \ 'down': '80%'
  \ }))
endfunction

" Get reference to recent files data (for testing)
function! webdav#recent#get_files()
  return s:recent_files
endfunction
