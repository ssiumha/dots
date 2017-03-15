autocmd! VimEnter <buffer> AnsiEsc

silent! execute '%s;[[:cntrl:]]\[7m;[34m;g'
silent! execute '%s;[[:cntrl:]]\[27m;[0m;g'

silent! execute 'g/^\* [[:cntrl:]]\[33mcommit/norm A {{{'
silent! execute 'g/^\* [[:cntrl:]]\[33mcommit/norm O }}}'
silent! execute '1s/}}}\n//'

silent! execute 'normal gg0'

setlocal nomodified

nnoremap <buffer> q ZQ
nnoremap <buffer> f <c-f>
nnoremap <buffer> b <c-b>
nnoremap <buffer> d <c-d>
nnoremap <buffer> u <c-u>
