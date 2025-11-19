" autoload/webdav/server.vim - Server configuration and URL parsing
" Functions for WebDAV server management and URL handling

" Get buffer's associated server name
" Returns server name from buffer or validates default server
function! webdav#server#get_buffer_info()
  let use_current_buffer = exists('b:webdav_managed') && b:webdav_managed
  if use_current_buffer
    return exists('b:webdav_server') ? b:webdav_server : ''
  else
    " Check if default server is configured
    let server_info = webdav#server#get_info('')
    if empty(server_info)
      return v:null
    endif
    return ''
  endif
endfunction

" Parse WebDAV URL with authentication
" Input: https://user:pass@host:port/path or http://host/path
" Returns: {'url': 'https://host:port/path', 'user': 'user', 'pass': 'pass'}
function! webdav#server#parse_url(url_string)
  let result = {'url': '', 'user': '', 'pass': ''}

  " Clean input to remove any newlines/whitespace
  let clean_url = webdav#core#clean_string(a:url_string)

  " Extract protocol
  let protocol_match = matchlist(clean_url, '^\([^:]\+\)://')
  if empty(protocol_match)
    return result
  endif
  let protocol = protocol_match[1]

  " Remove protocol from URL
  let rest = substitute(clean_url, '^\([^:]\+\)://', '', '')

  " Check for authentication (user:pass@)
  " Password can contain : (URL-encoded as %3A), so match everything between first : and last @
  let auth_match = matchlist(rest, '^\([^@:]\+\):\([^@]*\)@\(.*\)$')
  if !empty(auth_match)
    let result.user = webdav#core#clean_string(auth_match[1])
    let result.pass = webdav#core#clean_string(auth_match[2])
    let result.url = webdav#core#clean_string(protocol . '://' . auth_match[3])
  else
    " No authentication in URL
    let result.url = webdav#core#clean_string(protocol . '://' . rest)
  endif

  return result
endfunction

" Scan environment variables for WEBDAV_UI_* pattern
" Returns: Dictionary of {name: {url: 'url', user: 'user', 'pass': 'pass'}, ...}
function! webdav#server#scan()
  let servers = {}
  let prefix = 'WEBDAV_UI_'

  " Get all environment variables using Vim's environ()
  let env_vars = environ()

  for var_name in keys(env_vars)
    " Check if variable starts with WEBDAV_UI_
    if var_name =~# '^' . prefix
      " Extract server name (everything after prefix, converted to lowercase)
      let server_name = tolower(substitute(var_name, '^' . prefix, '', ''))

      " Parse URL with authentication (clean to remove any whitespace/newlines)
      let parsed = webdav#server#parse_url(webdav#core#clean_string(env_vars[var_name]))

      if !empty(parsed.url)
        let servers[server_name] = parsed
      endif
    endif
  endfor

  return servers
endfunction

" Get server info by server name
" Returns: Dictionary with 'url', 'user', 'pass' keys
" Priority: WEBDAV_UI_{NAME} -> WEBDAV_UI_DEFAULT -> WEBDAV_DEFAULT_* -> error
function! webdav#server#get_info(server_name)
  " If server name specified, scan for it
  if !empty(a:server_name)
    let servers = webdav#server#scan()
    if has_key(servers, a:server_name)
      return servers[a:server_name]
    endif
    echoerr "Server '" . a:server_name . "' not found in WEBDAV_UI_* environment variables"
    return {}
  endif

  " No server name - try WEBDAV_UI_DEFAULT first
  let servers = webdav#server#scan()
  if has_key(servers, 'default')
    return servers['default']
  endif

  " Fallback to WEBDAV_DEFAULT_* for backward compatibility (tests)
  let url = webdav#core#clean_string($WEBDAV_DEFAULT_URL)
  let user = webdav#core#clean_string($WEBDAV_DEFAULT_USER)
  let pass = webdav#core#clean_string($WEBDAV_DEFAULT_PASS)

  if !empty(url)
    return {'url': url, 'user': user, 'pass': pass}
  endif

  echoerr "No default server configured. Set WEBDAV_UI_DEFAULT or WEBDAV_DEFAULT_URL"
  return {}
endfunction
