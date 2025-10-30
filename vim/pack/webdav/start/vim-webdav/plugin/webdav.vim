if exists('g:loaded_webdav')
  finish
endif
let g:loaded_webdav = 1

let s:url = $WEBDAV_DEFAULT_URL
let s:user = $WEBDAV_DEFAULT_USER
let s:pass = $WEBDAV_DEFAULT_PASS

" Constants for HTTP response parsing
let s:HTTP_SEPARATOR_LEN = 4  " Length of \r\n\r\n
let s:UNIX_SEPARATOR_LEN = 2  " Length of \n\n
" Helper function to URL encode a string (but keep slashes)
function! s:URLEncode(str)
  " URL encode using perl but keep slashes
  let encoded = system("echo -n " . shellescape(a:str) . " | perl -MURI::Escape -e '$str = <STDIN>; print uri_escape($str, q{^A-Za-z0-9/._~-})'")
  return encoded
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

  " Set up buffer-local autocmd for :w (ONLY for this buffer)
  augroup webdav_save
    autocmd! * <buffer>
    autocmd BufWriteCmd <buffer> call s:WebDAVPut()
  augroup END

  " Mark as unmodified (just loaded)
  setlocal nomodified
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
    let cmd = 'curl -s -X PROPFIND -H "Depth: 1" ' . auth . ' ' . shellescape(url)
    let cmd .= ' | perl -MURI::Escape -e '
    let cmd .= shellescape('my $first = 1; my $base = ""; my @dirs = (); my @files = (); while (my $line = <STDIN>) { while ($line =~ /<D:href>([^<]*)<\/D:href>/g) { my $path = uri_unescape($1); if ($first) { $first = 0; $base = $path; next; } $path =~ s/^\Q$base\E//; if ($path =~ m{/$}) { push @dirs, $path; } else { push @files, $path; } } } foreach my $dir (sort @dirs) { print "$dir\n"; } foreach my $file (sort @files) { print "$file\n"; }')
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

  " Add header with full URL and parent directory
  call setline(1, '" WebDAV: ' . s:url . current_path)
  call append(1, '../')
  execute '2read !' . cmd

  " Remove empty lines and self-reference (.)
  execute 'silent! g/^\.\?$/d'
  " Path prefix removal is now handled in perl

  let b:webdav_current_path = current_path
  setlocal nomodifiable

  " Move cursor to top (first line)
  normal! gg

  " Key mappings for navigation
  nnoremap <buffer> <CR> :call <SID>WebDAVOpen()<CR>
endfunction

function! s:WebDAVOpen()
  let line = getline('.')
  if empty(trim(line)) || line =~ '^"'
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
  if empty(etag) && empty(last_modified)
    echoerr "Error: Server does not provide ETag or Last-Modified headers"
    echoerr "Cannot safely edit file without conflict detection"
    return
  endif

  " Setup buffer with content and metadata
  call s:SetupWebDAVBuffer(a:path, etag, last_modified, body)
endfunction

" Save WebDAV buffer with conflict detection
function! s:WebDAVPut()
  " Validate buffer is ready for save
  if !s:ValidateWebDAVBuffer()
    return
  endif

  try
    " Write buffer content to temp file
    let temp_file = tempname()
    call writefile(getline(1, '$'), temp_file, 'b')

    " Make conditional PUT request
    let etag = exists('b:webdav_etag') ? b:webdav_etag : ''
    let last_modified = exists('b:webdav_last_modified') ? b:webdav_last_modified : ''

    " SAFETY CHECK: Require version tracking to prevent data loss
    if empty(etag) && empty(last_modified)
      echoerr "Error: No version tracking info (ETag/Last-Modified)"
      echoerr "Cannot safely save without conflict detection"
      call delete(temp_file)
      return
    endif

    let cmd = s:WebDAVRequest('PUT', b:webdav_original_path, temp_file, etag, last_modified)
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
      " Success: Update version info and mark as unmodified
      call s:UpdateVersionInfo(response)
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

" Setup autocmd for saving WebDAV buffers (ONLY for WebDAV buffers)
augroup webdav_save
  autocmd!
  " Note: autocmd is set per-buffer in s:WebDAVGet()
augroup END
