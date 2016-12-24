let g:lightline = {
    \ 'colorscheme': 'seoul256',
    \ 'inactive' : {
    \   'left'  : [ [ 'filename' ] ],
    \   'right' : [ ]
    \ },
    \ 'active' : {
    \   'left'  : [ [ 'mode', 'paste' ],
    \               [ 'fugitive', 'readonly', 'filename', 'modified' ] ],
    \   'right' : [ [ 'percent', 'lineinfo' ],
    \               [ 'fileformat', 'fileencoding', 'filetype' ],
    \               [ 'tagbar' ] ]
    \ },
    \ 'component_function' : {
    \   'fugitive': 'MyFugitive'
    \ },
    \ 'component': {
    \   'tagbar': '%{tagbar#currenttag("%s", "")}',
    \ }
\ }


function! MyFugitive()
    return exists('*fugitive#head') ? fugitive#head() : ''
endfunction

function! StatuslineWhitespace()
    if search('\s\+$', 'nw') != 0 && mode() == 'n'
        return 'Trailing Whitespace'
    else
        return ''
    endif
endfunction
