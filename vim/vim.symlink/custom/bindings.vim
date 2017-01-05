"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Keybindings
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let mapleader=" "
let maplocalleader=","

inoremap jk <Esc>
nnoremap <silent> <Leader><Tab> :b#<cr>
nnoremap <silent> <Leader>bb    :FzfBuffers<cr>
nnoremap <silent> <Leader>bd    :bdelete<cr>
nnoremap <silent> <Leader>bw    :FixWhitespace<cr>

nnoremap <silent> <Leader>ff  :FzfFiles<cr>

nnoremap <silent> <Leader>sc :noh<cr>
nnoremap <silent> <Leader>so :OverCommandLine <cr>

nnoremap <silent> <Leader>gb :Gblame<cr>
nnoremap <silent> <Leader>gs :Gstatus<cr>
nnoremap <silent> <Leader>gc :Gcommit<cr>
nnoremap <silent> <Leader>gw :Gwrite<cr>

nnoremap <Leader>ws :spl<cr>
nnoremap <Leader>wS :vsp<cr>
nnoremap <Leader>wq :q<cr>

autocmd FileType c   nnoremap <C-]> :YcmCompleter GoTo<cr>
autocmd FileType cpp nnoremap <C-]> :YcmCompleter GoTo<cr>

" hit enter in visual mode to easy align
vmap <Enter> <Plug>(EasyAlign)

" use dvtm plugin
" nnoremap <silent> <C-h> :DvtmNavigateLeft<cr>
" nnoremap <silent> <C-j> :DvtmNavigateDown<cr>
" nnoremap <silent> <C-k> :DvtmNavigateUp<cr>
" nnoremap <silent> <C-l> :DvtmNavigateRight<cr>

if exists(':tnoremap')
    tnoremap jk <c-\><c-n>
endif

let g:move_key_modifier = 'C'

nmap gs  <plug>(GrepperOperator)
xmap gs  <plug>(GrepperOperator)
