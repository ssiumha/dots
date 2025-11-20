" autoload/webdav/http.vim - HTTP request/response handling
" Functions for WebDAV HTTP operations and response parsing

" HTTP response separator lengths
let s:HTTP_SEPARATOR_LEN = 4  " Length of \r\n\r\n
let s:UNIX_SEPARATOR_LEN = 2  " Length of \n\n


" Execute WebDAV request via system()
" Returns dictionary: {'response': string, 'success': boolean}
function! webdav#http#execute(cmd)
  let response = system(a:cmd)
  return {
    \ 'response': response,
    \ 'success': v:shell_error == 0
    \ }
endfunction

" Check if file exists on WebDAV server
" Returns: 1 (exists), 0 (doesn't exist), -1 (error)
function! webdav#http#file_exists(path, server_info)
  " Use PROPFIND with Depth: 0 to check if resource exists
  " Pass 1 for raw_response to get curl command with headers
  let cmd = webdav#http#build_request('PROPFIND', a:path, a:server_info, 1)
  if empty(cmd)
    return -1
  endif

  " Modify to use Depth: 0
  let cmd = substitute(cmd, '-H "Depth: 1"', '-H "Depth: 0"', '')

  let result = webdav#http#execute(cmd)
  if !result.success
    return -1
  endif
  let response = result.response

  " Parse HTTP status
  let http_code = webdav#core#extract_http_code(response)

  " 207 Multi-Status or 200 OK means file exists
  if http_code == 207 || (http_code >= 200 && http_code < 300)
    return 1
  endif

  " 404 Not Found means file doesn't exist
  if http_code == 404
    return 0
  endif

  " Other errors
  return -1
endfunction

" Extract header value from HTTP headers
function! webdav#http#extract_header(headers, header_name)
  let pattern = a:header_name . ': \zs[^\r\n]*'
  return matchstr(a:headers, pattern)
endfunction

" Parse HTTP response into headers and body
function! webdav#http#parse_response(response)
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

" Build curl WebDAV request command
" Parameters: method, path, server_info, ...
" For PUT: also pass temp_file, etag, last_modified as additional optional args
function! webdav#http#build_request(method, path, server_info, ...)
  let auth = empty(a:server_info.user) ? '' : '--user ' . shellescape(a:server_info.user . ':' . a:server_info.pass)
  " URL encode the path for all requests (handles Korean filenames, etc.)
  let encoded_path = webdav#core#url_encode(a:path)
  let url = a:server_info.url . encoded_path

  " DEBUG: Log URL construction
  call webdav#core#debug_log("DEBUG REQUEST: method=" . a:method)
  call webdav#core#debug_log("DEBUG REQUEST: server_url=" . string(a:server_info.url))
  call webdav#core#debug_log("DEBUG REQUEST: a:path=" . string(a:path))
  call webdav#core#debug_log("DEBUG REQUEST: encoded_path=" . string(encoded_path))
  call webdav#core#debug_log("DEBUG REQUEST: final url=" . string(url))

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
    let encoded_dest = webdav#core#url_encode(destination)
    let dest_url = a:server_info.url . encoded_dest

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

  " DEBUG: Log final command before execution
  call webdav#core#debug_log("DEBUG REQUEST: final cmd=" . string(cmd))

  return cmd
endfunction

" Check if folder is empty on WebDAV server
" Returns: 1 (empty), 0 (not empty), -1 (error)
function! webdav#http#folder_empty(path, server_name = '')
  " Use PROPFIND with Depth: 1 to list folder contents (raw response with headers)
  let server_info = webdav#server#get_info(a:server_name)
  let cmd = webdav#http#build_request('PROPFIND', a:path, server_info, 1)
  if empty(cmd)
    return -1
  endif

  let result = webdav#http#execute(cmd)
  if !result.success
    return -1
  endif
  let response = result.response

  " Parse HTTP status
  let http_code = webdav#core#extract_http_code(response)

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

" Check server version of file (get ETag and Last-Modified)
function! webdav#http#check_version(path, server_info)
  let result = {'last_modified': '', 'etag': '', 'error': ''}

  " Execute GET request to fetch current version info
  let cmd = webdav#http#build_request('GET', a:path, a:server_info)
  if empty(cmd)
    let result.error = 'Failed to build GET request'
    return result
  endif

  let req_result = webdav#http#execute(cmd)
  if !req_result.success
    let result.error = 'HTTP request failed: ' . req_result.response
    return result
  endif
  let response = req_result.response

  " Parse HTTP status
  let http_code = webdav#core#extract_http_code(response)

  if http_code < 200 || http_code >= 300
    let result.error = 'HTTP ' . http_code
    return result
  endif

  " Parse response and extract version headers
  let parsed = webdav#http#parse_response(response)
  let result.etag = webdav#http#extract_header(parsed.headers, 'ETag')
  let result.last_modified = webdav#http#extract_header(parsed.headers, 'Last-Modified')

  return result
endfunction
