
fun! UpFirst(val)
    let tmp = printf('%s', a:val)
    echo 'UpFirst '.tmp.' '.strlen(a:val).' '.strlen(tmp)
    let result = substitute(tmp,'.','\u\0','')
    "echo 'Result: '.result
    return result
endf

function! JavaTestFileName(type)
    let filepath = expand('%:p')
    let filepath = substitute(filepath, '/','.','g')
    let filepath = substitute(filepath, '^.\(:\\\)\?','','')
    let filepath = substitute(filepath, '\','.','g')
    let filepath = substitute(filepath, ' ','','g')
    let filepath = substitute(filepath, '.[A-Za-z_]*.java','','g')
    let filepath = substitute(filepath, '.*src[A-Za-z_]*\.','','g')
    return filepath
    if a:type == 1
        let filepath = substitute(filepath, '.[A-Za-z]*.java','','g')
    elseif a:type == 2
        let filepath = substitute(filepath, 'Tests.java','','')
    elseif a:type == 3
        let filepath = substitute(filepath, '.*\.\([A-Za-z]*\).java','\1','g')
    elseif a:type == 4
        let filepath = substitute(filepath, 'Tests.java','','')
        let filepath = substitute(filepath, '.*\.\([A-Za-z]*\).java','\1','g')
    elseif a:type == 5
        let filepath = substitute(filepath, 'Tests.java','','')
        let filepath = substitute(filepath, '.*\.\([A-Za-z]*\).java','\1','g')
        let filepath = substitute(filepath, '.','\l&','')
    endif

    return filepath
endfunction

inoremap <buffer> <C-Space> <C-X><C-U>

"TagList
"nnoremap <silent><LEADER>o :TlistToo<CR>
"inoremap <silent><LEADER>o <ESC>:TlistToo<CR>

inoremap <buffer> <C-A> <ESC>:JavaImpl<CR>
nnoremap <buffer> <C-A> :JavaImpl<CR>

inoremap <buffer> <C-Q> <ESC>:JavaCorrect<CR>
nnoremap <buffer> <C-Q> :JavaCorrect<CR>

inoremap <buffer> <silent><LEADER>z <C-O>:JavaImport<CR>
nnoremap <buffer> <silent><LEADER>z :JavaImport<CR>

inoremap <buffer> <silent><LEADER>x <C-O>:JavaImportMissing<CR>
nnoremap <buffer> <silent><LEADER>x :JavaImportMissing<CR>

inoremap <buffer> <silent><LEADER>g <ESC>:JavaGet<CR>
nnoremap <buffer> <silent><LEADER>g :JavaGet<CR>

"inoremap <buffer> <C-M> <ESC>:JavaImportMissing<CR>
"nnoremap <buffer> <C-M> :JavaImportMissing<CR>

vnoremap <buffer> <C-C> <ESC>:JavaConstructor<CR>
nnoremap <buffer> <C-C> :JavaConstructor<CR>
