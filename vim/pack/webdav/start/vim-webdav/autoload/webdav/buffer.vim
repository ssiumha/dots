" autoload/webdav/buffer.vim - Buffer management functions
" Functions for WebDAV buffer lifecycle management

" Validate that buffer is WebDAV-managed and ready for operations
" Returns: 1 if valid, 0 if not (with error message)
function! webdav#buffer#validate()
  " Check if this is a WebDAV-managed buffer
  if !exists('b:webdav_managed') || !b:webdav_managed
    echoerr "Error: Not a WebDAV buffer"
    return 0
  endif

  if !exists('b:webdav_original_path')
    echoerr "Error: WebDAV path not set"
    return 0
  endif

  " Get server info from buffer's server name
  let server_name = exists('b:webdav_server') ? b:webdav_server : ''
  let server_info = webdav#server#get_info(server_name)

  if empty(server_info.url)
    echoerr "Error: WEBDAV_DEFAULT_URL not set"
    return 0
  endif

  " Verify URL hasn't changed (prevent saving to wrong server)
  if exists('b:webdav_url') && b:webdav_url != server_info.url
    echoerr "Error: WebDAV URL mismatch. Cannot save to different server."
    echoerr "Original: " . b:webdav_url
    echoerr "Current:  " . server_info.url
    return 0
  endif

  return 1
endfunction

" Setup WebDAV buffer with metadata and save handler
" Parameters: path, server_name, server_info, etag, last_modified, body
function! webdav#buffer#setup(path, server_name, server_info, etag, last_modified, body)
  " Make buffer modifiable first (in case we're reusing a non-modifiable buffer)
  setlocal modifiable

  " Clear the buffer and prepare for writing
  %delete _

  " Ensure buftype is empty FIRST (before any content)
  setlocal buftype=
  setlocal noswapfile
  setlocal bufhidden=wipe

  " Load content into buffer
  call setline(1, split(a:body, '\n', 1))

  " Mark as WebDAV-managed buffer and store metadata
  let b:webdav_managed = 1
  let b:webdav_original_path = a:path
  let b:webdav_url = a:server_info.url
  let b:webdav_server = a:server_name  " Store server name (empty if direct access)
  let b:webdav_etag = a:etag
  let b:webdav_last_modified = a:last_modified

  " Set buffer name with webdav:// protocol to identify WebDAV buffers
  " This is required for BufWriteCmd to work properly
  let buffer_name = 'webdav://' . a:server_info.url . a:path
  " Escape % and # first (Vim special chars), then apply fnameescape
  let escaped_name = fnameescape(escape(buffer_name, '%#'))
  silent! execute 'file ' . escaped_name

  " Set filetype to webdav, but use syntax based on file extension
  setlocal filetype=webdav

  " Detect syntax from file extension
  let extension = fnamemodify(a:path, ':e')
  if !empty(extension)
    " Set syntax based on extension (markdown, ruby, python, etc.)
    let syntax_type = webdav#buffer#get_syntax(extension)
    if !empty(syntax_type)
      execute 'setlocal syntax=' . syntax_type
    endif
  endif

  " Set up buffer-local autocmd for :w (ONLY for this buffer)
  augroup webdav_save
    autocmd! * <buffer>
    autocmd BufWriteCmd <buffer> call webdav#file#put()
  augroup END

  " Mark as unmodified (just loaded)
  setlocal nomodified
endfunction

" Get syntax type from file extension
function! webdav#buffer#get_syntax(ext)
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

" Update buffer's version info (ETag/Last-Modified) from HTTP response
function! webdav#buffer#update_version(response)
  " Extract new ETag from response headers
  let new_etag = webdav#http#extract_header(a:response, 'ETag')
  if !empty(new_etag)
    let b:webdav_etag = new_etag
  endif

  " Extract new Last-Modified from response headers
  let new_modified = webdav#http#extract_header(a:response, 'Last-Modified')
  if !empty(new_modified)
    let b:webdav_last_modified = new_modified
  endif
endfunction

" Reload WebDAV buffer (called by :e! command via BufReadCmd)
function! webdav#buffer#reload()
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

  " Force reload by calling webdav#file#get
  " This will discard any unsaved changes (standard :e! behavior)
  call webdav#file#get(path)
endfunction
