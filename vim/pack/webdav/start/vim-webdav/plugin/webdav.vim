if exists('g:loaded_webdav')
  finish
endif
let g:loaded_webdav = 1

let s:url = $WEBDAV_DEFAULT_URL
let s:user = $WEBDAV_DEFAULT_USER
let s:pass = $WEBDAV_DEFAULT_PASS
let s:current_server = ''  " Name of currently connected server

" Constants for HTTP response parsing
let s:HTTP_SEPARATOR_LEN = 4  " Length of \r\n\r\n
let s:UNIX_SEPARATOR_LEN = 2  " Length of \n\n
" Helper function to URL encode a string (but keep slashes)
function! s:URLEncode(str)
  " URL encode using perl but keep slashes
  let encoded = system("echo -n " . shellescape(a:str) . " | perl -MURI::Escape -e '$str = <STDIN>; print uri_escape($str, q{^A-Za-z0-9/._~-})'")
  return encoded
endfunction

" Parse WebDAV URL with authentication
" Input: https://user:pass@host:port/path or http://host/path
" Returns: {'url': 'https://host:port/path', 'user': 'user', 'pass': 'pass'}
function! s:ParseWebDAVURL(url_string)
  let result = {'url': '', 'user': '', 'pass': ''}

  " Extract protocol
  let protocol_match = matchlist(a:url_string, '^\([^:]\+\)://')
  if empty(protocol_match)
    return result
  endif
  let protocol = protocol_match[1]

  " Remove protocol from URL
  let rest = substitute(a:url_string, '^\([^:]\+\)://', '', '')

  " Check for authentication (user:pass@)
  " Password can contain : (URL-encoded as %3A), so match everything between first : and last @
  let auth_match = matchlist(rest, '^\([^@:]\+\):\([^@]*\)@\(.*\)$')
  if !empty(auth_match)
    let result.user = auth_match[1]
    let result.pass = auth_match[2]
    let result.url = protocol . '://' . auth_match[3]
  else
    " No authentication in URL
    let result.url = protocol . '://' . rest
  endif

  return result
endfunction

" Scan environment variables for WEBDAV_UI_* pattern
" Returns: Dictionary of {name: {url: 'url', user: 'user', 'pass': 'pass'}, ...}
function! s:ScanWebDAVServers()
  let servers = {}
  let prefix = 'WEBDAV_UI_'

  " Get all environment variables using Vim's environ()
  let env_vars = environ()

  for var_name in keys(env_vars)
    " Check if variable starts with WEBDAV_UI_
    if var_name =~# '^' . prefix
      " Extract server name (everything after prefix, converted to lowercase)
      let server_name = tolower(substitute(var_name, '^' . prefix, '', ''))

      " Parse URL with authentication
      let parsed = s:ParseWebDAVURL(env_vars[var_name])

      if !empty(parsed.url)
        let servers[server_name] = parsed
      endif
    endif
  endfor

  return servers
endfunction

" Set current WebDAV server connection
function! s:SetCurrentServer(name, url, user, pass)
  let s:url = a:url
  let s:user = a:user
  let s:pass = a:pass
  let s:current_server = a:name
endfunction

" Interactive server selection UI
" Usage: WebDAVUI [server_name]
"   - No argument: Show interactive selection menu
"   - With argument: Directly connect to named server
function! s:WebDAVUI(...)
  " Scan for servers
  let servers = s:ScanWebDAVServers()

  " Check if any servers are configured
  if empty(servers)
    " Fallback to default environment variables if available
    if !empty($WEBDAV_DEFAULT_URL)
      let s:url = $WEBDAV_DEFAULT_URL
      let s:user = $WEBDAV_DEFAULT_USER
      let s:pass = $WEBDAV_DEFAULT_PASS
      let s:current_server = 'default'
      echo "No WEBDAV_UI_* servers found. Using WEBDAV_DEFAULT_* variables."
      call s:WebDAVList('/')
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
      call s:SetCurrentServer(server_name, server.url, server.user, server.pass)
      echo "Connected to '" . server_name . "' - " . server.url
      call s:WebDAVList('/')
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
  call s:SetCurrentServer(
    \ selected.name,
    \ selected.info.url,
    \ selected.info.user,
    \ selected.info.pass
  \ )

  " Automatically open root directory listing
  echo "\nConnected to '" . selected.name . "' - " . selected.info.url
  call s:WebDAVList('/')
endfunction

" Validate that WebDAV URL is configured
" Returns: 1 if valid, 0 if not (with error message)
function! s:ValidateURL()
  if empty(s:url)
    echo "Error: Set WEBDAV_DEFAULT_URL"
    return 0
  endif
  return 1
endfunction

" Validate that buffer is WebDAV-managed and ready for save
" Returns: 1 if valid, 0 if not (with error message)
function! s:ValidateWebDAVBuffer()
  " Check if this is a WebDAV-managed buffer
  if !exists('b:webdav_managed') || !b:webdav_managed
    echoerr "Error: Not a WebDAV buffer"
    return 0
  endif

  if !exists('b:webdav_original_path')
    echoerr "Error: WebDAV path not set"
    return 0
  endif

  if empty(s:url)
    echoerr "Error: WEBDAV_DEFAULT_URL not set"
    return 0
  endif

  " Verify URL hasn't changed (prevent saving to wrong server)
  if exists('b:webdav_url') && b:webdav_url != s:url
    echoerr "Error: WebDAV URL mismatch. Cannot save to different server."
    echoerr "Original: " . b:webdav_url
    echoerr "Current:  " . s:url
    return 0
  endif

  return 1
endfunction

" Extract a header value from HTTP headers
" Returns: Header value or empty string if not found
function! s:ExtractHeader(headers, header_name)
  let pattern = a:header_name . ': \zs[^\r\n]*'
  return matchstr(a:headers, pattern)
endfunction

" Setup WebDAV buffer with metadata and save handler
function! s:SetupWebDAVBuffer(path, etag, last_modified, body)
  " Make buffer modifiable first (in case we're reusing a non-modifiable buffer)
  setlocal modifiable

  " Clear the buffer and prepare for writing
  %delete _

  " Ensure buftype is empty FIRST (before any content)
  setlocal buftype=
  setlocal noswapfile
  setlocal bufhidden=hide

  " Load content into buffer
  call setline(1, split(a:body, '\n', 1))

  " Mark as WebDAV-managed buffer and store metadata
  let b:webdav_managed = 1
  let b:webdav_original_path = a:path
  let b:webdav_url = s:url
  let b:webdav_etag = a:etag
  let b:webdav_last_modified = a:last_modified

  " Set buffer name with webdav:// protocol to identify WebDAV buffers
  " This is required for BufWriteCmd to work properly
  let buffer_name = 'webdav://' . s:url . a:path
  silent! execute 'file ' . fnameescape(buffer_name)

  " Set filetype to webdav, but use syntax based on file extension
  setlocal filetype=webdav

  " Detect syntax from file extension
  let extension = fnamemodify(a:path, ':e')
  if !empty(extension)
    " Set syntax based on extension (markdown, ruby, python, etc.)
    let syntax_type = s:GetSyntaxFromExtension(extension)
    if !empty(syntax_type)
      execute 'setlocal syntax=' . syntax_type
    endif
  endif

  " Set up buffer-local autocmd for :w (ONLY for this buffer)
  augroup webdav_save
    autocmd! * <buffer>
    autocmd BufWriteCmd <buffer> call s:WebDAVPut()
  augroup END

  " Mark as unmodified (just loaded)
  setlocal nomodified
endfunction

" Get syntax type from file extension
function! s:GetSyntaxFromExtension(ext)
  let ext_map = {
    \ 'md': 'markdown',
    \ 'markdown': 'markdown',
    \ 'rb': 'ruby',
    \ 'py': 'python',
    \ 'js': 'javascript',
    \ 'ts': 'typescript',
    \ 'vim': 'vim',
    \ 'sh': 'sh',
    \ 'bash': 'bash',
    \ 'zsh': 'zsh',
    \ 'yaml': 'yaml',
    \ 'yml': 'yaml',
    \ 'json': 'json',
    \ 'xml': 'xml',
    \ 'html': 'html',
    \ 'css': 'css',
    \ 'txt': 'text',
    \ 'go': 'go',
    \ 'rs': 'rust',
    \ 'java': 'java',
    \ 'c': 'c',
    \ 'cpp': 'cpp',
    \ 'h': 'c',
    \ 'hpp': 'cpp',
  \ }

  return get(ext_map, tolower(a:ext), '')
endfunction

" Parse HTTP response into headers and body
" Returns: Dictionary with 'headers' and 'body' keys
function! s:ParseHTTPResponse(response)
  " Split headers and body (HTTP response format: headers\r\n\r\nbody)
  let separator_idx = match(a:response, "\r\n\r\n")
  if separator_idx == -1
    let separator_idx = match(a:response, "\n\n")
  endif

  if separator_idx != -1
    let headers = strpart(a:response, 0, separator_idx)
    let body = strpart(a:response, separator_idx + s:HTTP_SEPARATOR_LEN)
    if body[0:1] == "\n\n"
      let body = strpart(body, s:UNIX_SEPARATOR_LEN)
    endif
  else
    " No headers found, treat entire response as body
    let headers = ''
    let body = a:response
  endif

  return {'headers': headers, 'body': body}
endfunction

" Helper function to make curl WebDAV requests
" For PUT: pass temp_file, etag, last_modified as optional args
function! s:WebDAVRequest(method, path, ...)
  let auth = empty(s:user) ? '' : '--user ' . shellescape(s:user . ':' . s:pass)
  " URL encode the path for all requests (handles Korean filenames, etc.)
  let encoded_path = s:URLEncode(a:path)
  let url = s:url . encoded_path

  if a:method == 'PROPFIND'
    " Check if we need raw response (for checking folder contents)
    let raw_response = a:0 > 0 ? a:1 : 0

    if raw_response
      " Return raw response with headers for status checking
      let cmd = 'curl -i -s -X PROPFIND -H "Depth: 1" ' . auth . ' ' . shellescape(url)
    else
      " Return parsed file list (for directory listing)
      let cmd = 'curl -s -X PROPFIND -H "Depth: 1" ' . auth . ' ' . shellescape(url)
      let cmd .= ' | perl -MURI::Escape -e '
      let cmd .= shellescape('my $first = 1; my $base = ""; my @dirs = (); my @files = (); while (my $line = <STDIN>) { while ($line =~ /<D:href>([^<]*)<\/D:href>/g) { my $path = uri_unescape($1); if ($first) { $first = 0; $base = $path; next; } $path =~ s/^\Q$base\E//; if ($path =~ m{/$}) { push @dirs, $path; } else { push @files, $path; } } } foreach my $dir (sort @dirs) { print "$dir\n"; } foreach my $file (sort @files) { print "$file\n"; }')
    endif
  elseif a:method == 'GET'
    " Include headers with -i for version tracking
    let cmd = 'curl -i -s ' . auth . ' ' . shellescape(url)
  elseif a:method == 'PUT'
    " PUT with conditional headers for conflict detection
    let temp_file = a:0 > 0 ? a:1 : ''
    let etag = a:0 > 1 ? a:2 : ''
    let last_modified = a:0 > 2 ? a:3 : ''

    if empty(temp_file)
      echoerr "PUT requires temp file path"
      return ''
    endif

    let cmd = 'curl -i -s -X PUT ' . auth . ' '

    " Add conditional headers for optimistic locking
    if !empty(etag)
      let cmd .= '-H ' . shellescape('If-Match: ' . etag) . ' '
    elseif !empty(last_modified)
      let cmd .= '-H ' . shellescape('If-Unmodified-Since: ' . last_modified) . ' '
    endif

    let cmd .= '--data-binary @' . shellescape(temp_file) . ' '
    let cmd .= shellescape(url)
  elseif a:method == 'MKCOL'
    " MKCOL for creating collections (folders)
    let cmd = 'curl -i -s -X MKCOL ' . auth . ' ' . shellescape(url)
  elseif a:method == 'MOVE'
    " MOVE for renaming files/folders
    let destination = a:0 > 0 ? a:1 : ''
    if empty(destination)
      echoerr "MOVE requires destination path"
      return ''
    endif

    " Build destination URL with proper encoding
    let encoded_dest = s:URLEncode(destination)
    let dest_url = s:url . encoded_dest

    let cmd = 'curl -i -s -X MOVE ' . auth . ' '
    let cmd .= '-H ' . shellescape('Destination: ' . dest_url) . ' '
    let cmd .= shellescape(url)
  elseif a:method == 'DELETE'
    " DELETE for removing files/folders
    let cmd = 'curl -i -s -X DELETE ' . auth . ' ' . shellescape(url)
  else
    echoerr "Unknown HTTP method: " . a:method
    return ''
  endif

  return cmd
endfunction

function! s:WebDAVList(path = '/')
  if !s:ValidateURL()
    return
  endif

  " Use curl for WebDAV PROPFIND
  let current_path = a:path
  let cmd = s:WebDAVRequest('PROPFIND', a:path)

  setlocal buftype=nofile bufhidden=wipe
  setlocal modifiable

  " Clear buffer if it has content (for refresh)
  if line('$') > 1 || len(getline(1)) > 0
    silent! %delete _
  endif

  " Add header with full URL, action items, and parent directory
  call setline(1, '" WebDAV: ' . s:url . current_path)
  call append(1, '../')
  call append(2, '+New')
  call append(3, '+Folder')
  execute '$read !' . cmd

  " Remove empty lines and self-reference (.)
  execute 'silent! g/^\.\?$/d'
  " Path prefix removal is now handled in perl

  let b:webdav_current_path = current_path

  " Set filetype AFTER content is loaded
  setlocal filetype=webdavlist
  setlocal nomodifiable

  " Move cursor to top (first line)
  normal! gg

  " Key mappings for navigation
  nnoremap <buffer> <CR> :call <SID>WebDAVOpen()<CR>
  nnoremap <buffer> R :call <SID>WebDAVRename()<CR>
  nnoremap <buffer> D :call <SID>WebDAVDelete()<CR>
endfunction

function! s:WebDAVOpen()
  let line = getline('.')
  if empty(trim(line)) || line =~ '^"'
    return
  endif

  " Handle special action items
  if line == '+New'
    call s:WebDAVCreateNewFile(b:webdav_current_path)
    return
  elseif line == '+Folder'
    call s:WebDAVCreateNewFolder(b:webdav_current_path)
    return
  endif

  " Handle parent directory
  if line == '../'
    let current = b:webdav_current_path
    let parent = substitute(current, '/[^/]*/$', '/', '')
    tabnew
    call s:WebDAVList(parent)
    return
  endif

  " Build full path from current path and relative name
  let path = b:webdav_current_path . line

  " Check if it's a directory (ends with /)
  if line =~ '/$'
    " Open directory in new tab
    tabnew
    call s:WebDAVList(path)
  else
    " Open file in new tab
    tabnew
    call s:WebDAVGet(path)
  endif
endfunction

" Create new file in current directory
function! s:WebDAVCreateNewFile(current_path)
  if !s:ValidateURL()
    return
  endif

  " Prompt for file name
  let filename = input('New file name (without extension): ')
  if empty(trim(filename))
    echo "\nCancelled"
    return
  endif

  " Add .md extension automatically
  let filename = trim(filename) . '.md'

  " Build full path
  let file_path = a:current_path . filename

  " Create initial content with title
  let title = substitute(filename, '\.md$', '', '')
  let initial_content = '# ' . title

  try
    " Write initial content to temp file
    let temp_file = tempname()
    call writefile([initial_content], temp_file, 'b')

    " Create file on server via PUT (no ETag for new file)
    let cmd = s:WebDAVRequest('PUT', file_path, temp_file, '', '')
    if empty(cmd)
      call delete(temp_file)
      return
    endif

    let response = system(cmd)
    call delete(temp_file)

    if v:shell_error != 0
      echoerr "Error creating file: HTTP request failed"
      echoerr response
      return
    endif

    " Parse response to check success
    let status_line = matchstr(response, '^HTTP/[^ ]* \zs\d\+')
    let http_code = str2nr(status_line)

    if http_code >= 200 && http_code < 300
      echo "\nCreated: " . filename

      " Fetch version info from server (nginx may not return ETag/Last-Modified in PUT response)
      let version_info = s:CheckServerVersion(file_path)
      let etag = empty(version_info.error) ? version_info.etag : ''
      let last_modified = empty(version_info.error) ? version_info.last_modified : ''

      " Fallback to extracting from PUT response if GET fails
      if empty(etag) && empty(last_modified)
        let parsed = s:ParseHTTPResponse(response)
        let etag = s:ExtractHeader(parsed.headers, 'ETag')
        let last_modified = s:ExtractHeader(parsed.headers, 'Last-Modified')
      endif

      " Open new file in editor
      tabnew
      call s:SetupWebDAVBuffer(file_path, etag, last_modified, initial_content)
    else
      echoerr "Error creating file: HTTP " . http_code
      echoerr response
    endif
  catch
    echoerr "Exception creating file: " . v:exception
  endtry
endfunction

" Create new folder in current directory
function! s:WebDAVCreateNewFolder(current_path)
  if !s:ValidateURL()
    return
  endif

  " Prompt for folder name
  let foldername = input('New folder name: ')
  if empty(trim(foldername))
    echo "\nCancelled"
    return
  endif

  " Ensure folder name ends with /
  let foldername = trim(foldername)
  if foldername !~ '/$'
    let foldername .= '/'
  endif

  " Build full path
  let folder_path = a:current_path . foldername

  " Create folder via MKCOL
  let cmd = s:WebDAVRequest('MKCOL', folder_path)
  if empty(cmd)
    return
  endif

  let response = system(cmd)

  if v:shell_error != 0
    echoerr "Error creating folder: HTTP request failed"
    echoerr response
    return
  endif

  " Parse response to check success
  let status_line = matchstr(response, '^HTTP/[^ ]* \zs\d\+')
  let http_code = str2nr(status_line)

  if http_code >= 200 && http_code < 300
    echo "\nCreated folder: " . foldername
    " Refresh current list
    call s:WebDAVList(a:current_path)
  else
    echoerr "Error creating folder: HTTP " . http_code
    echoerr response
  endif
endfunction

" Rename (move) file or folder in current directory
function! s:WebDAVRename()
  if !s:ValidateURL()
    return
  endif

  " Get current line item
  let line = getline('.')
  if empty(trim(line)) || line =~ '^"'
    echo "Cannot rename special items"
    return
  endif

  " Skip special action items
  if line == '+New' || line == '+Folder' || line == '../'
    echo "Cannot rename special items"
    return
  endif

  " Determine if it's a folder (ends with /)
  let is_folder = (line =~ '/$')
  let current_name = line

  " Build source path
  let source_path = b:webdav_current_path . current_name

  " Prompt for new name with current name as default
  let prompt = is_folder ? 'Rename folder to: ' : 'Rename file to: '
  let new_name = input(prompt, current_name)
  if empty(trim(new_name)) || trim(new_name) == current_name
    echo "\nCancelled"
    return
  endif

  let new_name = trim(new_name)

  " Ensure folder name ends with /
  if is_folder && new_name !~ '/$'
    let new_name .= '/'
  endif

  " Build destination path
  " Support moving to different directories (e.g., "nested/file.txt" or "/other/path/file.txt")
  if new_name =~ '^/'
    " Absolute path
    let dest_path = new_name
  else
    " Relative path (could include subdirectories)
    let dest_path = b:webdav_current_path . new_name
  endif

  " Check if destination already exists
  let check_cmd = s:WebDAVRequest('PROPFIND', dest_path)
  if !empty(check_cmd)
    " Modify PROPFIND to only check if resource exists (Depth: 0)
    let check_cmd = substitute(check_cmd, '-H "Depth: 1"', '-H "Depth: 0"', '')
    let check_response = system(check_cmd)

    if v:shell_error == 0
      " Parse HTTP status
      let status_line = matchstr(check_response, '^HTTP/[^ ]* \zs\d\+')
      let http_code = str2nr(status_line)

      if http_code == 207 || (http_code >= 200 && http_code < 300)
        " Destination exists - ask for confirmation
        echohl WarningMsg
        echo "\nDestination already exists: " . new_name
        echohl None
        let choice = confirm('Overwrite?', "&Yes\n&No", 2)
        if choice != 1
          echo "\nCancelled"
          return
        endif
      endif
    endif
  endif

  " Execute MOVE request
  let cmd = s:WebDAVRequest('MOVE', source_path, dest_path)
  if empty(cmd)
    return
  endif

  let response = system(cmd)

  if v:shell_error != 0
    echoerr "Error renaming: HTTP request failed"
    echoerr response
    return
  endif

  " Parse response status
  let status_line = matchstr(response, '^HTTP/[^ ]* \zs\d\+')
  let http_code = str2nr(status_line)

  if http_code >= 200 && http_code < 300
    echo "\nRenamed: " . current_name . " -> " . new_name
    " Refresh current list
    call s:WebDAVList(b:webdav_current_path)
  elseif http_code == 409
    echoerr "Error: Parent collection does not exist or conflict occurred"
    echoerr response
  elseif http_code == 412
    echoerr "Error: Precondition failed (destination may already exist)"
    echoerr response
  else
    echoerr "Error renaming: HTTP " . http_code
    echoerr response
  endif
endfunction

function! s:WebDAVGet(path)
  if !s:ValidateURL()
    return
  endif

  " Use curl for WebDAV GET (with headers)
  let cmd = s:WebDAVRequest('GET', a:path)

  " Get response with headers (no need to escape % for system() call)
  let response = system(cmd)
  if v:shell_error != 0
    echoerr "Error: HTTP request failed"
    echoerr response
    return
  endif

  " Parse HTTP response
  let parsed = s:ParseHTTPResponse(response)
  let headers = parsed.headers
  let body = parsed.body

  " Extract version information from headers
  let etag = s:ExtractHeader(headers, 'ETag')
  let last_modified = s:ExtractHeader(headers, 'Last-Modified')

  " SAFETY CHECK: Require at least one version tracking mechanism
  " Exception: Allow empty files without version tracking
  if empty(etag) && empty(last_modified)
    if !empty(body)
      echoerr "Error: Server does not provide ETag or Last-Modified headers"
      echoerr "Cannot safely edit file without conflict detection"
      return
    endif
    " Empty file - proceed without version tracking (safe since no data to lose)
  endif

  " Setup buffer with content and metadata
  call s:SetupWebDAVBuffer(a:path, etag, last_modified, body)
endfunction

" Check if folder is empty (for safe deletion)
" Returns: 1 if empty, 0 if has contents, -1 on error
function! s:CheckFolderEmpty(path)
  " Use PROPFIND with Depth: 1 to list folder contents (raw response with headers)
  let cmd = s:WebDAVRequest('PROPFIND', a:path, 1)
  if empty(cmd)
    return -1
  endif

  let response = system(cmd)
  if v:shell_error != 0
    return -1
  endif

  " Parse HTTP status
  let status_line = matchstr(response, '^HTTP/[^ ]* \zs\d\+')
  let http_code = str2nr(status_line)

  if http_code != 207
    return -1
  endif

  " Count <D:response> elements in PROPFIND response
  " First response is the folder itself, additional responses are contents
  let response_count = len(split(response, '<D:response>', 1)) - 1

  " If only 1 response (the folder itself), it's empty
  " If more than 1 response, it has contents
  return (response_count <= 1) ? 1 : 0
endfunction

" Delete file or empty folder from WebDAV server
function! s:WebDAVDelete()
  " SAFETY: Only work in webdavlist buffers
  if &filetype != 'webdavlist'
    return
  endif

  if !s:ValidateURL()
    return
  endif

  " Get current line item
  let line = getline('.')
  if empty(trim(line)) || line =~ '^"'
    echo "Cannot delete special items"
    return
  endif

  " SAFETY: Block deletion of special items
  if line == '+New' || line == '+Folder' || line == '../'
    echo "Cannot delete special items"
    return
  endif

  " Determine if it's a folder (ends with /)
  let is_folder = (line =~ '/$')
  let item_name = line

  " Build path
  let path = b:webdav_current_path . item_name

  " SAFETY: For folders, check if empty
  if is_folder
    echo "Checking if folder is empty..."
    let empty_status = s:CheckFolderEmpty(path)

    if empty_status == -1
      echoerr "Error: Could not verify folder status"
      return
    endif

    if empty_status == 0
      echoerr "Error: Folder is not empty. Delete contents first."
      return
    endif
  endif

  " Confirmation prompt (skip in test mode)
  if !exists('$WEBDAV_TEST_MODE') || $WEBDAV_TEST_MODE != '1'
    let choice = confirm('Delete "' . item_name . '"?', "&Yes\n&No", 2)
    if choice != 1
      echo "Cancelled"
      return
    endif
  endif

  " Execute DELETE request
  let cmd = s:WebDAVRequest('DELETE', path)
  if empty(cmd)
    return
  endif

  let response = system(cmd)

  if v:shell_error != 0
    echoerr "Error deleting: HTTP request failed"
    echoerr response
    return
  endif

  " Parse response status
  let status_line = matchstr(response, '^HTTP/[^ ]* \zs\d\+')
  let http_code = str2nr(status_line)

  if http_code >= 200 && http_code < 300
    echo "\nDeleted: " . item_name
    " Refresh current list
    call s:WebDAVList(b:webdav_current_path)
  elseif http_code == 404
    echoerr "Error: Item not found"
  elseif http_code == 409
    echoerr "Error: Folder is not empty or has dependencies"
    echoerr response
  else
    echoerr "Error deleting: HTTP " . http_code
    echoerr response
  endif
endfunction

" Reload WebDAV buffer (called by :e! command via BufReadCmd)
function! s:WebDAVReload()
  " Extract path from buffer name (webdav://<url><path>)
  let buffer_name = expand('%')

  " Verify this is a WebDAV buffer
  if buffer_name !~# '^webdav://'
    echoerr "Error: Not a WebDAV buffer"
    return
  endif

  " Verify buffer is managed
  if !exists('b:webdav_managed') || !b:webdav_managed
    echoerr "Error: Not a WebDAV-managed buffer"
    return
  endif

  " Get the original path from buffer variable
  if !exists('b:webdav_original_path')
    echoerr "Error: WebDAV path not found"
    return
  endif

  let path = b:webdav_original_path

  " Force reload by calling WebDAVGet
  " This will discard any unsaved changes (standard :e! behavior)
  call s:WebDAVGet(path)
endfunction

" Check server version by fetching current Last-Modified and ETag
" Returns: Dictionary with 'last_modified', 'etag', and 'error' keys
function! s:CheckServerVersion(path)
  let result = {'last_modified': '', 'etag': '', 'error': ''}

  " Execute GET request to fetch current version info
  let cmd = s:WebDAVRequest('GET', a:path)
  if empty(cmd)
    let result.error = 'Failed to build GET request'
    return result
  endif

  let response = system(cmd)
  if v:shell_error != 0
    let result.error = 'HTTP request failed: ' . response
    return result
  endif

  " Parse HTTP status
  let status_line = matchstr(response, '^HTTP/[^ ]* \zs\d\+')
  let http_code = str2nr(status_line)

  if http_code < 200 || http_code >= 300
    let result.error = 'HTTP ' . http_code
    return result
  endif

  " Parse response and extract version headers
  let parsed = s:ParseHTTPResponse(response)
  let result.etag = s:ExtractHeader(parsed.headers, 'ETag')
  let result.last_modified = s:ExtractHeader(parsed.headers, 'Last-Modified')

  return result
endfunction

" Save WebDAV buffer with conflict detection
function! s:WebDAVPut()
  " Validate buffer is ready for save
  if !s:ValidateWebDAVBuffer()
    return
  endif

  try
    " Get our stored version info
    let our_etag = exists('b:webdav_etag') ? b:webdav_etag : ''
    let our_last_modified = exists('b:webdav_last_modified') ? b:webdav_last_modified : ''

    " SAFETY CHECK: Require version tracking to prevent data loss
    if empty(our_etag) && empty(our_last_modified)
      echohl WarningMsg
      echo "Warning: No version tracking (no ETag/Last-Modified)"
      echo "Cannot detect conflicts - save may overwrite other changes"
      echohl None
      let proceed = input('Proceed with save anyway? (y/N): ')
      if tolower(proceed) != 'y'
        echo "\nSave cancelled"
        return
      endif
    endif

    " Check server version before PUT (manual conflict detection for nginx)
    if !empty(our_etag) || !empty(our_last_modified)
      echo "Checking server version..."
      let server_version = s:CheckServerVersion(b:webdav_original_path)

      if !empty(server_version.error)
        echoerr "Warning: Could not check server version: " . server_version.error
        let proceed = input('Proceed with save anyway? (y/N): ')
        if tolower(proceed) != 'y'
          echo "\nSave cancelled"
          return
        endif
      else
        " Compare versions (prefer ETag if both sides have it, otherwise use Last-Modified)
        let conflict_detected = 0

        if !empty(our_etag) && !empty(server_version.etag)
          " Compare ETags
          if our_etag != server_version.etag
            let conflict_detected = 1
          endif
        elseif !empty(our_last_modified) && !empty(server_version.last_modified)
          " Compare Last-Modified timestamps
          if our_last_modified != server_version.last_modified
            let conflict_detected = 1
          endif
        endif

        if conflict_detected
          echohl WarningMsg
          echo "Conflict: Server file has been modified by another client"
          echohl None
          echo "Your version: " . (empty(our_etag) ? our_last_modified : our_etag)
          echo "Server version: " . (empty(server_version.etag) ? server_version.last_modified : server_version.etag)
          echo ""
          echo "Options:"
          echo "  o - Overwrite server version with your changes (your changes win)"
          echo "  c - Cancel save and open server version for manual merge"
          echo "  any other key - Cancel save"
          let choice = nr2char(getchar())
          echo "\n"

          if choice == 'o' || choice == 'O'
            echo "Overwriting server version..."
          elseif choice == 'c' || choice == 'C'
            echo "Opening server version for manual merge..."
            call s:WebDAVResolveConflict()
            return
          else
            echo "Save cancelled"
            return
          endif
        endif
      endif
    endif

    " Write buffer content to temp file
    let temp_file = tempname()
    call writefile(getline(1, '$'), temp_file, 'b')

    " Make PUT request (without conditional headers since nginx ignores them)
    " We've already done manual conflict detection above
    let cmd = s:WebDAVRequest('PUT', b:webdav_original_path, temp_file, '', '')
    if empty(cmd)
      return
    endif

    " Execute PUT request
    let response = system(cmd)
    if v:shell_error != 0
      echoerr "Error: HTTP request failed"
      echoerr response
      return
    endif

    " Parse HTTP status line and headers
    let status_line = matchstr(response, '^HTTP/[^ ]* \zs\d\+')
    let http_code = str2nr(status_line)

    " Handle response
    if http_code >= 200 && http_code < 300
      " Success: Fetch fresh version info after save
      " This is necessary because nginx may not return updated ETag/Last-Modified in PUT responses
      let fresh_version = s:CheckServerVersion(b:webdav_original_path)
      if empty(fresh_version.error)
        let b:webdav_etag = fresh_version.etag
        let b:webdav_last_modified = fresh_version.last_modified
      else
        " Fallback to extracting from PUT response if GET fails
        call s:UpdateVersionInfo(response)
      endif

      setlocal nomodified
      echo "Saved to WebDAV: " . b:webdav_original_path
    elseif http_code == 412
      " Conflict: Server file was modified
      echoerr "Conflict: Server file has been modified"
      echo "Opening latest version in new tab..."
      call s:WebDAVResolveConflict()
    elseif http_code >= 400 && http_code < 500
      echoerr "Error saving to WebDAV: HTTP " . http_code . " (Client Error)"
    elseif http_code >= 500
      echoerr "Error saving to WebDAV: HTTP " . http_code . " (Server Error)"
    else
      echoerr "Error saving to WebDAV: HTTP " . http_code
    endif
  catch
    echoerr "Exception saving to WebDAV: " . v:exception
  finally
    " Cleanup temp file
    if exists('temp_file') && filereadable(temp_file)
      call delete(temp_file)
    endif
  endtry
endfunction

" Update version info from PUT response headers
function! s:UpdateVersionInfo(response)
  " Extract new ETag from response headers
  let new_etag = s:ExtractHeader(a:response, 'ETag')
  if !empty(new_etag)
    let b:webdav_etag = new_etag
  endif

  " Extract new Last-Modified from response headers
  let new_modified = s:ExtractHeader(a:response, 'Last-Modified')
  if !empty(new_modified)
    let b:webdav_last_modified = new_modified
  endif
endfunction

" Resolve conflict by opening server version in new tab
function! s:WebDAVResolveConflict()
  if !exists('b:webdav_original_path')
    echoerr "Error: WebDAV path not found"
    return
  endif

  let original_path = b:webdav_original_path

  " Open server version in new tab
  tabnew
  call s:WebDAVGet(original_path)

  echo "Conflict resolution: Left tab (previous) = your local changes, Right tab (current) = server version"
  echo "Manually merge changes and save again"
endfunction

command! -nargs=? WebDAVList call s:WebDAVList(<q-args>)
command! -nargs=1 WebDAVGet call s:WebDAVGet(<q-args>)
command! WebDAVPut call s:WebDAVPut()
command! -nargs=? WebDAVUI call s:WebDAVUI(<q-args>)

" Setup autocmd for WebDAV buffers (ONLY for webdav:// protocol buffers)
augroup webdav_buffers
  autocmd!
  " Handle :e! (force reload) for WebDAV buffers only
  " This ONLY affects buffers with webdav:// protocol - normal files are unaffected
  autocmd BufReadCmd webdav://* call s:WebDAVReload()
  " Note: BufWriteCmd is set per-buffer in s:SetupWebDAVBuffer()
augroup END
