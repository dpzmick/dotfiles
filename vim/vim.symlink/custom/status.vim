let g:lightline = {
    \ 'colorscheme': 'solarized',
    \ 'inactive' : {
    \   'left'  : [ [ 'filename' ] ],
    \   'right' : [ ]
    \ },
    \ 'active' : {
    \   'left'  : [ [ 'mode', 'paste' ],
    \               [ 'fugitive', 'readonly', 'filename', 'modified' ] ],
    \   'right' : [ [ 'percent', 'lineinfo' ],
    \               [ 'fileformat', 'fileencoding', 'filetype' ],
    \               [ 'syntastic', 'whitespace' ] ]
    \ },
    \ 'component_expand' : {
    \   'syntastic' : 'SyntasticStatuslineFlag'
    \ },
    \ 'component_type' : {
    \   'syntastic' : 'error'
    \ },
    \ 'component_function' : {
    \   'fugitive': 'MyFugitive',
    \   'whitespace' : 'StatuslineWhitespace'
    \ }
\ }


function! MyFugitive()
    return exists('*fugitive#head') ? fugitive#head() : ''
endfunction

" augroup AutoSyntastic
"     autocmd!
"     autocmd BufWritePost *.c,*.cpp call s:syntastic()
" augroup END
" function! s:syntastic()
"     SyntasticCheck
"     call lightline#update()
" endfunction

function! StatuslineWhitespace()
    if search('\s\+$', 'nw') != 0 && mode() == 'n'
        return 'Trailing Whitespace'
    else
        return ''
    endif
endfunction
