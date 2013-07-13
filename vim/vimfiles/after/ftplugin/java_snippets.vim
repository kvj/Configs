if !exists('loaded_snippet') || &cp
    finish
endif

function! UpFirst()
    return substitute(@z,'.','\u&','')
endfunction

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

let st = g:snip_start_tag
let et = g:snip_end_tag
let cd = g:snip_elem_delim

exec "Snippet psf private static final ".st."string".et." ".st.et." = ".st.et.";"
exec "Snippet psfi private static final int ".st.et." = ".st.et.";"
exec "Snippet trc try {<CR>".st.et."<CR>} catch (Throwable t) {<CR>t.printStackTrace();<CR>}<CR>"
exec "Snippet jlog /** Logger for this class and subclasses. */<CR><CR>protected final Log log = LogFactory.getLog(getClass());<CR>".st.et
exec "Snippet sout System.out.println(\"".st.et."\");"
exec "Snippet jtest package ".st."j:JavaTestFileName(1)".et."<CR><CR>import junit.framework.TestCase;<CR>import ".st."j:JavaTestFileName(2)".et.";<CR><CR>/**<CR> * ".st."j:JavaTestFileName(3)".et."<CR> *<CR> * @author ".st.et."<CR> * @since ".st.et."<CR> */<CR>public class ".st."j:JavaTestFileName(3)".et." extends TestCase {<CR><CR>private ".st."j:JavaTestFileName(4)".et." ".st."j:JavaTestFileName(5)".et.";<CR><CR>public ".st."j:JavaTestFileName(4)".et." get".st."j:JavaTestFileName(4)".et."() { return this.".st."j:JavaTestFileName(5)".et."; }<CR>public void set".st."j:JavaTestFileName(4)".et."(".st."j:JavaTestFileName(4)".et." ".st."j:JavaTestFileName(5)".et.") { this.".st."j:JavaTestFileName(5)".et." = ".st."j:JavaTestFileName(5)".et."; }<CR><CR>public void test".st.et."() {<CR>".st.et."<CR>}<CR>}<CR>".st.et
exec "Snippet main public static void main(String[] args) {<CR>".st."\"System.exit(0)\"".et.";<CR>}<CR>"
exec "Snippet pu public ".st.et
exec "Snippet pr private ".st.et
exec "Snippet St String ".st.et
exec "Snippet im implements ".st.et
exec "Snippet ex extends ".st.et
exec "Snippet iof instanceof ".st.et
exec "Snippet st static ".st.et
exec "Snippet tr true".st.et
exec "Snippet fa false".st.et
exec "Snippet imp import ".st.et.";<CR>"
exec "Snippet inc public ".st."int".et." get".st."field:UpFirst()".et."() {<CR>return ".st."field".et.";<CR>}<CR><CR>public void set".st."field:UpFirst()".et."(".st."int".et." ".st."field".et.") {<CR>this.".st."field".et." = ".st."field".et.";<CR>}<CR>"
exec "Snippet cl public class ".st."class".et." {<CR><CR>public ".st."class".et."(".st.et.") {<CR>}<CR>}"
exec "Snippet ic public interface ".st."iface".et." {<CR>}"
exec "Snippet if if (".st.et.") {<CR>}"
exec "Snippet el else {<CR>".st.et."<CR>}"
exec "Snippet for for (var ".st."i".et." = 0; ".st."i".et." < ".st.et."; ".st."i".et."++) {<CR>".st.et."<CR>}"
exec "Snippet fore for (".st."type".et." ".st."var".et.": ".st."enum".et.") {<CR>".st.et."<CR>}"
exec "Snippet pa package ".st."j:JavaTestFileName(1)".et.";"

