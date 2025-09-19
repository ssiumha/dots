if exists('g:loaded_webdav')
  finish
endif
let g:loaded_webdav = 1

let s:url = $WEBDAV_DEFAULT_URL
let s:user = $WEBDAV_DEFAULT_USER
let s:pass = $WEBDAV_DEFAULT_PASS
" Helper function to URL encode a string (but keep slashes)
function! s:URLEncode(str)
  " URL encode using perl but keep slashes
  let encoded = system("echo -n " . shellescape(a:str) . " | perl -MURI::Escape -e '$str = <STDIN>; print uri_escape($str, q{^A-Za-z0-9/._~-})'")
  return encoded
endfunction

" Helper function to make curl WebDAV requests
function! s:WebDAVRequest(method, path)
  let auth = empty(s:user) ? '' : '--user ' . shellescape(s:user . ':' . s:pass)
  " URL encode the path for GET requests
  let encoded_path = (a:method == 'GET') ? s:URLEncode(a:path) : a:path
  let url = s:url . encoded_path

  if a:method == 'PROPFIND'
    let cmd = 'curl -s -X PROPFIND -H "Depth: 1" ' . auth . ' ' . shellescape(url)
    let cmd .= ' | perl -MURI::Escape -e '
    let cmd .= shellescape('my $first = 1; my $base = ""; my @dirs = (); my @files = (); while (my $line = <STDIN>) { while ($line =~ /<D:href>([^<]*)<\/D:href>/g) { my $path = uri_unescape($1); if ($first) { $first = 0; $base = $path; next; } $path =~ s/^\Q$base\E//; if ($path =~ m{/$}) { push @dirs, $path; } else { push @files, $path; } } } foreach my $dir (sort @dirs) { print "$dir\n"; } foreach my $file (sort @files) { print "$file\n"; }')
  elseif a:method == 'GET'
    let cmd = 'curl -s ' . auth . ' ' . shellescape(url)
  endif

  return cmd
endfunction

function! s:WebDAVList(path = '/')
  if empty(s:url)
    echo "Error: Set WEBDAV_DEFAULT_URL"
    return
  endif

  " Use curl for WebDAV PROPFIND
  let current_path = a:path
  let cmd = s:WebDAVRequest('PROPFIND', a:path)

  setlocal buftype=nofile bufhidden=wipe

  " Add header and parent directory
  call setline(1, '" WebDAV: ' . current_path)
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
  if empty(s:url)
    echo "Error: Set WEBDAV_DEFAULT_URL"
    return
  endif

  " Use curl for WebDAV GET
  let cmd = s:WebDAVRequest('GET', a:path)

  setlocal buftype=nofile
  " Escape % characters for Vim
  let escaped_cmd = substitute(cmd, '%', '\\%', 'g')
  execute 'read !' . escaped_cmd
  1delete _
endfunction

command! -nargs=? WebDAVList call s:WebDAVList(<q-args>)
command! -nargs=1 WebDAVGet call s:WebDAVGet(<q-args>)
