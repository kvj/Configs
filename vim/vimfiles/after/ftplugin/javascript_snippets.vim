if !exists('loaded_snippet') || &cp
    finish
endif

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

"exec "Snippet proto ".st."className".et.".prototype.".st."methodName".et." = function(".st.et.")<CR>{<CR>".st.et."<CR>};<CR>".st.et
exec "Snippet fun function(".st.et.") {//".st.et."<CR>".st.et."<CR><BS><BS><BS><BS><BS><BS><BS><BS>}"
"exec "Snippet fun function(".st.et.") {//".st.et."{}<Left><CR><CR><Up><Tab>".st.et
exec "Snippet { {<CR>".st.et."<CR><BS><BS><BS><BS><BS><BS><BS><BS>}"
exec "Snippet $ $('".st.et."')".st."\".\"".et
exec "Snippet trc try {//".st.et."<CR>} catch (e) {//".st.et."<CR>}<CR>"
exec "Snippet al alert('".st.et."');"
exec "Snippet va var ".st."i".et." = ".st.et.";"
exec "Snippet log log('".st.et."');"
exec "Snippet tr true".st.et
exec "Snippet fa false".st.et
exec "Snippet if if (".st.et.") {//".st.et."<CR>}"
exec "Snippet wh while (".st.et.") {//".st.et."<CR>}"
exec "Snippet el else {//".st.et."<CR>}"
exec "Snippet for for (var ".st."i".et." = 0; ".st."i".et." < ".st.et."; ".st."i".et."++) {//".st.et."<CR>}"
exec "Snippet fore for (var ".st."id".et." in ".st."arr".et.") {//".st.et."<CR>}"
