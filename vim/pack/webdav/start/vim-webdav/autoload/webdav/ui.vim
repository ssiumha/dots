" autoload/webdav/ui.vim - Navigation and UI functions
" Functions for server selection, directory listing, and file/folder navigation

" Interactive server selection menu
" Parameters: optional server_name (variadic)
function! webdav#ui#main(...)
  " Scan for servers
  let servers = webdav#server#scan()

  " Check if any servers are configured
  if empty(servers)
    " Fallback to default environment variables if available
    if !empty($WEBDAV_DEFAULT_URL)
      echo "No WEBDAV_UI_* servers found. Using WEBDAV_DEFAULT_* variables."
      call webdav#ui#list('/', '')
      return
    else
      echoerr "Error: No WebDAV servers configured."
      echoerr "Set WEBDAV_UI_<NAME>=https://user:pass@host/path environment variables"
      return
    endif
  endif

  " Check if server name was provided as argument
  let server_name = a:0 > 0 ? trim(a:1) : ''

  if !empty(server_name)
    " Direct selection by name
    if has_key(servers, server_name)
      let server = servers[server_name]
      echo "Connected to '" . server_name . "' - " . server.url
      call webdav#ui#list('/', server_name)
      return
    else
      " Server not found - show available servers
      echoerr "Error: Server '" . server_name . "' not found."
      echo "Available servers: " . join(sort(keys(servers)), ', ')
      return
    endif
  endif

  " No argument - show interactive selection
  " Build selection list
  let choices = ["Select WebDAV Server:"]
  let server_list = []
  let idx = 1

  for name in sort(keys(servers))
    let server = servers[name]
    let display_url = server.url
    " Mask password in display
    let display_user = empty(server.user) ? '' : ' (' . server.user . '@...)'
    call add(choices, idx . '. ' . name . ': ' . display_url . display_user)
    call add(server_list, {'name': name, 'info': server})
    let idx += 1
  endfor

  " Show interactive selection
  let selection = inputlist(choices)

  " Validate selection
  if selection < 1 || selection > len(server_list)
    echo "\nCancelled or invalid selection."
    return
  endif

  " Set selected server as current
  let selected = server_list[selection - 1]

  " Automatically open root directory listing
  echo "\nConnected to '" . selected.name . "' - " . selected.info.url
  call webdav#ui#list('/', selected.name)
endfunction

" Display directory listing for WebDAV path
" Parameters: path (default '/'), server_name (default '')
function! webdav#ui#list(path = '/', server_name = '')
  " Determine server to use
  if !empty(a:server_name)
    " Use explicitly provided server
    let server_name = a:server_name
    let current_path = a:path
  elseif exists('b:webdav_managed') && b:webdav_managed
    " Use current buffer's directory and server
    if exists('b:webdav_original_path')
      " WebDAV file buffer - extract directory from file path
      let file_path = b:webdav_original_path
      let current_path = a:path == '/' ? substitute(file_path, '[^/]*$', '', '') : a:path
    elseif exists('b:webdav_current_path')
      " WebDAVList buffer - use current path directly
      let current_path = a:path == '/' ? b:webdav_current_path : a:path
    else
      let current_path = a:path
    endif
    let server_name = exists('b:webdav_server') ? b:webdav_server : ''
  else
    let current_path = a:path
    let server_name = ''
  endif

  let server_info = webdav#server#get_info(server_name)
  if empty(server_info)
    return
  endif
  let cmd = webdav#http#build_request('PROPFIND', current_path, server_info)

  setlocal buftype=nofile bufhidden=wipe
  setlocal modifiable

  " Clear buffer if it has content (for refresh)
  if line('$') > 1 || len(getline(1)) > 0
    silent! %delete _
  endif

  " Add header with full URL, action items, and parent directory
  call setline(1, '" WebDAV: ' . server_info.url . current_path)
  call append(1, '../')
  call append(2, '+New')
  call append(3, '+Folder')
  execute '$read !' . cmd

  " Remove empty lines and self-reference (.)
  execute 'silent! g/^\.\?$/d'
  " Path prefix removal is now handled in perl

  let b:webdav_current_path = current_path
  let b:webdav_server = server_name
  let b:webdav_managed = 1

  " Set filetype AFTER content is loaded
  setlocal filetype=webdavlist
  setlocal nomodifiable

  " Move cursor to top (first line)
  normal! gg
endfunction

" Handle opening files/folders from list buffer (Enter key handler)
function! webdav#ui#open()
  let line = getline('.')
  if empty(trim(line)) || line =~ '^"'
    return
  endif

  " Handle special action items
  if line == '+New'
    call webdav#operations#create_file(b:webdav_current_path)
    return
  elseif line == '+Folder'
    call webdav#operations#create_folder(b:webdav_current_path)
    return
  endif

  " Get current buffer's server info to pass to new tab
  let current_server = exists('b:webdav_server') ? b:webdav_server : ''

  " Handle parent directory
  if line == '../'
    let current = b:webdav_current_path
    " Remove trailing slash if present, then remove last path component
    let clean_path = substitute(current, '/$', '', '')
    let parent = substitute(clean_path, '/[^/]*$', '/', '')
    " Ensure parent ends with /
    if parent !~ '/$'
      let parent .= '/'
    endif
    call webdav#ui#list(parent, current_server)
    return
  endif

  " Build full path from current path and relative name
  let path = webdav#core#join_path(b:webdav_current_path, line)

  " Check if it's a directory (ends with /)
  if line =~ '/$'
    " Open directory in current buffer
    call webdav#ui#list(path, current_server)
  else
    " Open file in current buffer
    call WebDAVGet(path, current_server)
  endif
endfunction

" Open in new tab (t key handler)
function! webdav#ui#open_in_tab()
  let line = getline('.')
  if empty(trim(line)) || line =~ '^"'
    return
  endif

  " Handle special action items
  if line == '+New'
    call webdav#operations#create_file(b:webdav_current_path)
    return
  elseif line == '+Folder'
    call webdav#operations#create_folder(b:webdav_current_path)
    return
  endif

  let current_server = exists('b:webdav_server') ? b:webdav_server : ''

  " Handle parent directory
  if line == '../'
    let current = b:webdav_current_path
    let clean_path = substitute(current, '/$', '', '')
    let parent = substitute(clean_path, '/[^/]*$', '/', '')
    if parent !~ '/$'
      let parent .= '/'
    endif
    tabnew
    call webdav#ui#list(parent, current_server)
    return
  endif

  let path = webdav#core#join_path(b:webdav_current_path, line)

  if line =~ '/$'
    tabnew
    call webdav#ui#list(path, current_server)
  else
    tabnew
    call WebDAVGet(path, current_server)
  endif
endfunction

" Go to parent directory (- key handler)
function! webdav#ui#go_up()
  let current = b:webdav_current_path
  let current_server = exists('b:webdav_server') ? b:webdav_server : ''

  " Remove trailing slash
  let clean_path = substitute(current, '/$', '', '')

  " Get parent directory
  let parent = substitute(clean_path, '/[^/]*$', '/', '')

  " Ensure it ends with /
  if parent !~ '/$'
    let parent .= '/'
  endif

  " Open parent in current buffer
  call webdav#ui#list(parent, current_server)
endfunction
