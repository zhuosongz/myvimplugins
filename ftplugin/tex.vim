call IMAP('eeee', "\\begin{equation*}\<CR><++>\<CR>\\end{equation*}<++>",'tex')
call IMAP('pppp', "\\begin{align}\<CR><++>\<CR>\\end{align}<++>",'tex')
call IMAP('pppl', "\\begin{align*}\<CR><++>\<CR>\\end{align*}<++>",'tex')
call IMAP('ppp[', "align*\<F5>",'tex')
call IMAP("EA*", "\<C-r>=Tex_PutEnvironment('align*')\<CR>", 'tex')
call IMAP('LLL', "\\label{<++>}<++>",'tex')
"call IMAP('CC', "\\cref{<++>}<++>",'tex')
call IMAP('eqq',"\\eqref{<++>}<++>",'tex')
call IMAP('cqq',"\\cref{<++>}<++>",'tex')
call IMAP('Cqq',"\\Cref{<++>}<++>",'tex')

call IMAP('e\\',"\\E\\big\\lvert<++>\\big\\rvert<++>",'tex')
call IMAP('b\\',"\\E\\Big\\lvert<++>\\Big\\rvert<++>",'tex')
call IMAP('ttt',"\\text{<++>}<++>",'tex')
call IUNMAP('==', 'tex')
call IMAP ('==', '& = ', "tex")
call IMAP ('<<', '& \le ', "tex")
call IMAP ('>>', '& \ge ', "tex")
call IMAP('`o', '\omega', 'tex')
let g:Tex_Com_ll = "\\label{<++>}<++>"
let g:Tex_Com_cpp = "\\conep{<++>}{<++>}<++>" 
let g:Tex_Com_cpb = "\\conepb{<++>}{<++>}<++>" 
let g:Tex_Com_cpbb = "\\conepbb{<++>}{<++>}<++>" 
let g:Tex_Com_cpB = "\\conepB{<++>}{<++>}<++>" 
let g:Tex_Com_cpBB = "\\conepBB{<++>}{<++>}<++>" 
let g:Tex_Env_ppp = "\\begin{align*}\<CR><++>\<CR>\\end{align*}<++>"

" imap ;o \omega 
" imap ;v \phi 
" imap ;i \vee 
"imap ;1 \uparrow 
"imap ;3 \downarrow 
"imap ;4 \leftarrow 
"imap ;5 \rightarrow 
set winaltkeys=no
set macmeta

let g:Tex_AdvancedMath = 1
let g:Tex_Leader = ';'
let g:tex_flavor='latex'


set wrap
imap <C-b> <ESC><ESC>:w<CR>\ll:only<CR>\ls
nmap <C-b> <ESC>:w<CR>\ll:only<CR>\ls
imap ;j <ESC><Plug>IMAP_JumpForward
nmap ;j <ESC><Plug>IMAP_JumpForward

imap <A-a> <ESC>:w<CR>:!pdflatex % && asy %:r-*.asy && pdflatex % && open %:r.pdf<CR><CR>
nmap <A-a> <ESC>:w<CR>:!pdflatex % && asy %:r-*.asy && pdflatex % && open %:r.pdf<CR><CR>


imap <F8> <ESC><ESC>Bi\cref{<ESC>Ea}
nmap <F8> <ESC>Bi\cref{<ESC>Ea}
imap <F10> <ESC><ESC>Bi\label{<ESC>Ea}
nmap <F10> <ESC>Bi\label{<ESC>Ea}
imap <F3> <ESC><ESC>I\pushright{<ESC>$a}<ESC>
nmap <F3> <ESC>I\pushright{<ESC>$a}<ESC>
" imap pppl align*<F5>


" compile the tex file with the following method
" view pdf by Skim
"let g:Tex_MultipleCompileFormats='pdf,bib,pdf,pdf'
let g:Tex_MultipleCompileFormats='pdf'
let g:Tex_ViewRule_pdf = 'Skim'

" Add argument (can be negative, default 1) to global variable i.
" Return value of i before the change.



" Map <A-g/v> as \Bigl( <++> \Bigr)<++> 
function! Tex_BigLR()
	let line = getline(line("."))
	let char = line[col(".")-2]
	let previous = line[col(".")-3]

	let matchedbrackets = '()[]{}|?'
	if char =~ '(\|\[\|{\||\|?'
		let add = ''
		if char =~ '{'
			let add = "\\"
		endif
		let rhs = matchstr(matchedbrackets, char.'\zs.\ze')
        if char =~ '|'
            let add = '\'
            let char='lvert'
            let rhs='rvert'
        endif
        if char =~ '?'
            let add = '\'
            let char='lVert'
            let rhs='rVert'
        endif
		return "\<BS>".IMAP_PutTextWithMovement('\Bigl'.add.char.'<++>\Bigr'.add.rhs.'<++>')
    else
        return ''
    endif 
endfunction " }}}

inoremap <silent> <Plug>Tex_BigLR      <C-r>=Tex_BigLR()<CR>
imap <A-G> <Plug>Tex_BigLR


" Map <A-g> as \bigl( \bigr)
function! Tex_bigLR()
	let line = getline(line("."))
	let char = line[col(".")-2]
	let previous = line[col(".")-3]

	let matchedbrackets = '()[]{}|?'
	if char =~ '(\|\[\|{\||\|?'
		let add = ''
		if char =~ '{'
			let add = "\\"
		endif
		let rhs = matchstr(matchedbrackets, char.'\zs.\ze')
        if char =~ '|'
            let add = '\'
            let char='lvert'
            let rhs='rvert'
        endif
        if char =~ '?'
            let add = '\'
            let char='lVert'
            let rhs='rVert'
        endif
		return "\<BS>".IMAP_PutTextWithMovement('\bigl'.add.char.'<++>\bigr'.add.rhs.'<++>')
    else
        return ''
    endif 
endfunction " }}}

inoremap <silent> <Plug>Tex_bigLR      <C-r>=Tex_bigLR()<CR>
imap <A-g> <Plug>Tex_bigLR

" Map <A-v> as \bigl( \bigr)
function! Tex_biggLR()
	let line = getline(line("."))
	let char = line[col(".")-2]
	let previous = line[col(".")-3]

	let matchedbrackets = '()[]{}|?'
	if char =~ '(\|\[\|{\||\|?'
		let add = ''
		if char =~ '{'
			let add = "\\"
		endif
		let rhs = matchstr(matchedbrackets, char.'\zs.\ze')
        if char =~ '|'
            let add = '\'
            let char='lvert'
            let rhs='rvert'
        endif
        if char =~ '?'
            let add = '\'
            let char='lVert'
            let rhs='rVert'
        endif
		return "\<BS>".IMAP_PutTextWithMovement('\biggl'.add.char.'<++>\biggr'.add.rhs.'<++>')
    else
        return ''
    endif 
endfunction " }}}

inoremap <silent> <Plug>Tex_biggLR      <C-r>=Tex_biggLR()<CR>
imap <A-v> <Plug>Tex_biggLR


" map @a <ESC>"kdi)V:s/\(.*\)()\(.*\)/\1 \\bigl( \\bigr) \2/g<CR>/\\bigl( \\bigr)<CR> f "kp


let g:Tex_PackagesMenu=0
let g:Tex_NestElementMenus=0
" let g:Tex_UseUtfMenus=0
" 
let g:Tex_Menus=0
set gfn=Monaco:h15
let g:Tex_package_detected = ''
let g:Tex_SmartKeyDot=1


" no autopairs

let b:AutoPairs = {}

" let exists('g:AutoPairsLoaded')=0
"
let g:vimtex_delim_toggle_mod_list = [
      \ ['\bigl', '\bigr'],
      \ ['\Bigl', '\Bigr'],
      \ ['\biggl', '\biggr'],
      \ ['\Biggl', '\Biggr'],
      \]



set selection=inclusive
behave xterm
let g:vimtex_quickfix_mode=1
set spell

" ====================================================================================================
" What does this mean?
" Example: 
"   \begin{align*}
"       f(x) + g(x) = h(x).    
"       \label{ex-1}
"   \end{align*}
"
"   By lse, we comment the label part
"   By Lse, we remove the commant. 

nmap lse tsevie:s/\(\\label\)/% \1/g<CR>
nmap lsE cseequ<CR>[moll<F7>

" ====================================================================================================
"
"
" ====================================================================================================
" What does this mean? 
"  f ( I ) --- by cs(U, we get ---> f \{ I \} 
"          --- by cs(u, we get ---> f \[ I \]
"
"   Package required: *surrounder*
"
let b:surround_{char2nr('c')} = "\\\1command: \1{\r}"
let b:surround_{char2nr('C')} = "{\\\1command: \1 \r}"
let b:surround_85 = "\\{\r\\}" 
" 85 is ascii of U
let g:surround_86 = "\\lVert \r\\rVert" 
" 86 is ascii of V
let g:surround_118 = "\\lvert \r\\rvert" 
" 118 is ascii of v
let g:surround_117 = "\\[\r\\]"
" 117 is ascii of u
"" existing capability
""let g:surround_{char2nr("v")} = "‘\r’"
""let g:surround_{char2nr("V")} = "“\r”"

"" hypothetical custom targets
""let g:surr_target_{char2nr("v")} = ["\\lvert","\\rvert"]
"let g:surr_target_118 = ["\\lvert","\\rvert"]
"let g:surr_target_{char2nr("V")} = ['\lVert','\rVert']

"
"
" existing capability
let g:surround_{char2nr("q")} = "`\r'"
let g:surround_{char2nr("Q")} = "``\r''"

" hypothetical custom targets
" let g:surr_target_{char2nr("q")} = ['`',"\'"]
" let g:surr_target_{char2nr("Q")} = ['``',"\'\'"]
" hypothetical custom targets
" let g:surr_target_{char2nr("q")} = ['‘','’']
" let g:surr_target_{char2nr("Q")} = ['“','”']



call textobj#user#plugin('latex', {
\   'environment': {
\     '*pattern*': ['\\begin{[^}]\+}.*\n\s*', '\n^\s*\\end{[^}]\+}.*$'],
\     'select-a': 'ae',
\     'select-i': 'ie',
\   },
\  'bracket-math': {
\     '*pattern*': ['\\\[', '\\\]'],
\     'select-a': 'au',
\     'select-i': 'iu',
\   },
\  'paren-math': {
\     '*pattern*': ['\\(', '\\)'],
\     'select-a': 'a\',
\     'select-i': 'i\',
\   },
\  'dollar-math-a': {
\     '*pattern*': '[$][^$]*[$]',
\     'select': 'a$',
\   },
\  'dollar-math-i': {
\     '*pattern*': '[$]\zs[^$]*\ze[$]',
\     'select': 'i$',
\   },
\  'quote': {
\     '*pattern*': ['`', "'"],
\     'select-a': 'aq',
\     'select-i': 'iq',
\   },
\  'double-quote': {
\     '*pattern*': ['``', "''"],
\     'select-a': 'aQ',
\     'select-i': 'iQ',
\   },
\  'biglimiter': {
\     '*pattern*': ['\\{', '\\}'],
\     'select-a': 'aU',
\     'select-i': 'iU',
\   },
\ })

" ====================================================================================================


" ====================================================================================================

set iskeyword+=-
let g:Tex_EnvEndWithCR=0
let g:Tex_EnvLabelprefix_equation = "eq:\\thesec"
let g:Tex_EnvLabelprefix_equ = "eq:\\thesec"
let g:Tex_EnvLabelprefix_align = "eq:\\thesec"
let g:Tex_EnvLabelprefix_theorem = "thm:\\thesec"
let g:Tex_EnvLabelprefix_lemma = "lem:\\thesec"
let g:Tex_EnvLabelprefix_figure = "fig:\\thesec"
let g:Tex_EnvLabelprefix_table = "tab:\\thesec"
let g:Tex_EnvLabelprefix_definition = "def:\\thesec"
let g:Tex_EnvLabelprefix_proposition = "pro:\\thesec"
let g:Tex_EnvLabelprefix_corollary = "cor:\\thesec"
let g:Tex_EnvLabelprefix_assumption = "ass:\\thesec"
let g:Tex_EnvLabelprefix_remark = "rem:\\thesec"
let g:Tex_LabelAfterContent=0
let g:Tex_GotoError=0

set foldmethod=manual

" call s:Tex_EnvMacros('EUL', '&Math.', 'equ')
" call s:Tex_EnvMacros('EUS', '&Math.', 'equ*')


"call g:TexNewMathZone("E","equ",1)


""48-57 stands the numbers 0-9
" set iskeyword+=48
"
" Setting for tex-preview
"autocmd Filetype tex setl updatetime=1
" let g:livepreview_previewer = 'open -a Skim'
" let g:livepreview_previewer = 'evince'
" let g:livepreview_previewer = 'mupdf'
"let g:livepreview_previewer = 'open -a Preview'
"function ComplieWithXeTeX()
"    let oldRule = g:Tex_CompileRule_pdf
"    let g:Tex_CompileRule_pdf = 'latexmk --synctex=-1 -src-specials -interaction=nonstopmode $*'
"    call Tex_RunLaTeX()
"    let g:Tex_CompileRule_pdf = oldRule
"endfunction
"map <Leader>lx :<C-U>call ComplieWithXeTeX()<CR>
"
"function CleanTempFiles()
"    execute '!latexmk -c'
"
"endfunction
"

"function! Tex_RunLaTeX()
"	call Tex_Debug('+Tex_RunLaTeX, b:fragmentFile = '.exists('b:fragmentFile'), 'comp')

"	let dir = expand("%:p:h").'/'
"	let l:origdir = fnameescape(getcwd())
"	call Tex_CD(expand("%:p:h"))

"	let initTarget = s:target

"	" first get the dependency chain of this format.
"	call Tex_Debug("Tex_RunLaTeX: compiling to target [".s:target."]", "comp")

"	if Tex_GetVarValue('Tex_FormatDependency_'.s:target) != ''
"		let dependency = Tex_GetVarValue('Tex_FormatDependency_'.s:target)
"		if dependency !~ ','.s:target.'$'
"			let dependency = dependency.','.s:target
"		endif
"	else
"		let dependency = s:target
"	endif

"	call Tex_Debug('Tex_RunLaTeX: getting dependency chain = ['.dependency.']', 'comp')

"	" now compile to the final target format via each dependency.
"	let i = 1
"	while Tex_Strntok(dependency, ',', i) != ''
"		let s:target = Tex_Strntok(dependency, ',', i)

"		call Tex_SetTeXCompilerTarget('Compile', s:target)
"		call Tex_Debug('Tex_RunLaTeX: setting target to '.s:target, 'comp')

"		if Tex_GetVarValue('Tex_MultipleCompileFormats') =~ '\<'.s:target.'\>'
"			call Tex_Debug("Tex_RunLaTeX: compiling file multiple times via Tex_CompileMultipleTimes", "comp")
"			call Tex_CompileMultipleTimes()
"		else
"			call Tex_Debug("Tex_RunLaTeX: compiling file once via Tex_CompileLatex", "comp")
"			call Tex_CompileLatex()
"		endif

"		let errlist = Tex_GetErrorList()
"		"call Tex_Debug("Tex_RunLaTeX: errlist = [".errlist."]", "comp")

"		" If there are any errors, then break from the rest of the steps
"		if errlist =~  '\v(error|warning)'
"			"call Tex_Debug('Tex_RunLaTeX: There were errors in compiling, breaking chain...', 'comp')
"			break
"		endif

"		let i = i + 1
"	endwhile

"	let s:target = initTarget
"	let s:origwinnum = winnr()
"	"call Tex_SetupErrorWindow()

"	exe 'cd '.l:origdir
"	call Tex_Debug("-Tex_RunLaTeX", "comp")
"endfunction


function! IsLineEmpty(line)
    return match(a:line, "^\\s*$") != -1
endfunction

function! Tex_AutoFile()
    "write
    let l:curlineabc = getline(line("."))
    let l:is_math = match(map(synstack(line('.'), max([col('.') - 1, 1])),
        \ 'synIDattr(v:val, ''name'')'), '^texMathZone[A-Z]S\?$') >= 0
    " if IsLineEmpty(l:curlineabc) && l:is_math
    "     s/^\(\s*\)$/\1%/
    " endif 
    if l:is_math==0
        call Tex_CompileLatex() 
    elseif IsLineEmpty(l:curlineabc)==0
        call Tex_CompileLatex()
    endif 
endfunction


function! Tex_pdfbibpdf()
    write
    !pdflatex %
    !bibtex %:t:r.aux
    !pdflatex %
    !pdflatex %
endfunction
map <Leader>lb :silent call Tex_pdfbibpdf()<CR>
"<CR><CR><CR>
let g:tex_flavor='pdflatex'
let g:vimtex_quickfix_open_on_warning = 0
let g:TCLevel = 1
"let g:Tex_CompileRule_pdf='pdflatex $*'


nmap <Leader>up :source ~/Dropbox/config/vim/.vim/ftplugin/tex.vim<CR>

"let autosave=10
"imap \pp <ESC>:w<CR>
"nmap \pp <ESC>:w<CR> 
"nmap `ll \ll
" augroup runtex
"     au!
"     au FileWritePost *.tex call Tex_AutoFile() 
" augroup end
"autocmd FileWritePost *.tex call Tex_RunLaTeX() 
"| call Tex_ViewLaTeX()
"call Tex_CompileMultipleTimes()   "
"<CR>\ll
"call Tex_CompileMultipleTimes()
"<ESC>\ll\lc\lv\ls
"!pdflatex % && open -a Skim %:r.pdf 
"
"
set updatetime=5000
"autocmd CursorHold *.tex silent call Tex_AutoFile()
autocmd CursorHold *.tex silent write
autocmd CursorHoldI *.tex silent write
"autocmd CursorHoldI *.tex silent call Tex_AutoFile()
"autocmd InsertLeave *.tex silent write 
"autocmd CursorMovedI *.tex silent call Tex_AutoFile()
"autocmd CursorMoved *.tex silent call Tex_AutoFile()
let g:Imap_UsePlaceHolders = 1 
let g:Imap_PlaceHolderStart = '{{~'
let g:Imap_PlaceHolderEnd = '~}}'


let g:matchup_override_vimtex = 1
let g:vimtex_quickfix_mode = 0


"hi MatchParen cterm=underline ctermbg=none ctermfg=magenta
colorscheme mylatexcolor

"function! vimtex#delim#change(open, close, new) " {{{1
"  "
"  " Set target environment
"  "
"  if a:new ==# ''
"    let [l:beg, l:end] = ['', '']
"  elseif a:new ==# 'lr('
"    let [l:beg, l:end] = ['\left(', '\right)']
"  elseif a:new ==# 'b('
"    let [l:beg, l:end] = ['\bigl(', '\bigr)']
"  elseif a:new ==# 'B('
"    let [l:beg, l:end] = ['\Bigl(', '\Bigr)']
"  elseif a:new ==# 'bb('
"    let [l:beg, l:end] = ['\biggl(', '\biggr)']
"  elseif a:new ==# 'BB('
"    let [l:beg, l:end] = ['\Biggl(', '\Biggr)']
"  elseif a:new ==# 'lr['
"    let [l:beg, l:end] = ['\left[', '\right]']
"  elseif a:new ==# 'b['
"    let [l:beg, l:end] = ['\bigl[', '\bigr]']
"  elseif a:new ==# 'B['
"    let [l:beg, l:end] = ['\Bigl[', '\Bigr]']
"  elseif a:new ==# 'bb['
"    let [l:beg, l:end] = ['\biggl[', '\biggr]']
"  elseif a:new ==# 'BB['
"    let [l:beg, l:end] = ['\Biggl[', '\Biggr]']
"  elseif a:new ==# 'lr{'
"    let [l:beg, l:end] = ['\left\{', '\right\}']
"  elseif a:new ==# 'b{'
"    let [l:beg, l:end] = ['\bigl\{', '\bigr\}']
"  elseif a:new ==# 'B{'
"    let [l:beg, l:end] = ['\Bigl\{', '\Bigr\}']
"  elseif a:new ==# 'bb{'
"    let [l:beg, l:end] = ['\biggl\{', '\biggr\}']
"  elseif a:new ==# 'BB{'
"    let [l:beg, l:end] = ['\Biggl\{', '\Biggr\}']
"  elseif a:new ==# 'lr|'
"    let [l:beg, l:end] = ['\left\lvert', '\right\rvert']
"  elseif a:new ==# 'b|'
"    let [l:beg, l:end] = ['\bigl\lvert', '\bigr\rvert']
"  elseif a:new ==# 'B|'
"    let [l:beg, l:end] = ['\Bigl\lvert', '\Bigr\rvert']
"  elseif a:new ==# 'bb|'
"    let [l:beg, l:end] = ['\biggl\lvert', '\biggr\rvert']
"  elseif a:new ==# 'BB|'
"    let [l:beg, l:end] = ['\Biggl\lvert', '\Biggr\rvert']
"  elseif a:new ==# 'lr||'
"    let [l:beg, l:end] = ['\left\lVert', '\right\rVert']
"  elseif a:new ==# 'b||'
"    let [l:beg, l:end] = ['\bigl\lVert', '\bigr\rVert']
"  elseif a:new ==# 'B||'
"    let [l:beg, l:end] = ['\Bigl\lVert', '\Bigr\rVert']
"  elseif a:new ==# 'bb||'
"    let [l:beg, l:end] = ['\biggl\lVert', '\biggr\rVert']
"  elseif a:new ==# 'BB||'
"    let [l:beg, l:end] = ['\Biggl\lVert', '\Biggr\rVert']
"  " elseif a:new ==# 'b('
"  "   let [l:beg, l:end] = ['\bigl(', '\bigr)']
"  else
"    let l:side = a:new =~# g:vimtex#delim#re.delim_all.close
"    let l:index = index(map(copy(g:vimtex#delim#lists.delim_all.name),
"          \   'v:val[' . l:side . ']'),
"          \ a:new)
"    if l:index >= 0
"      let [l:beg, l:end] = g:vimtex#delim#lists.delim_all.name[l:index]
"    else
"      let [l:beg, l:end] = [a:new, a:new]
"    endif
"  endif

"  let l:line = getline(a:open.lnum)
"  call setline(a:open.lnum,
"        \   strpart(l:line, 0, a:open.cnum-1)
"        \ . l:beg
"        \ . strpart(l:line, a:open.cnum + len(a:open.match) - 1))

"  let l:c1 = a:close.cnum
"  let l:c2 = a:close.cnum + len(a:close.match) - 1
"  if a:open.lnum == a:close.lnum
"    let n = len(l:beg) - len(a:open.match)
"    let l:c1 += n
"    let l:c2 += n
"    let pos = vimtex#pos#get_cursor()
"    if pos[2] > a:open.cnum + len(a:open.match) - 1
"      let pos[2] += n
"      call vimtex#pos#set_cursor(pos)
"    endif
"  endif

"  let l:line = getline(a:close.lnum)
"  call setline(a:close.lnum,
"        \ strpart(l:line, 0, l:c1-1) . l:end . strpart(l:line, l:c2))

"  if a:new ==# ''
"    silent! call repeat#set("\<plug>(vimtex-delim-delete)", v:count)
"  else
"    silent! call repeat#set(
"          \ "\<plug>(vimtex-delim-change-math)" . a:new . '
', v:count)
"  endif
"endfunction


"nmap <A-o> V:s/\%\(\,\s*\)\@<!\([.,]\)\(\s*$\|\s*\\\\\)/\\,\1\2/<CR>
let @d = 'V:s/\%\(\,\s*\)\@<!\([.,]\)\(\s*$\|\s*\\\\\)/\\,\1\2/'
nmap <A-o> @d<CR>


let Tex_FoldedEnvironments = ",proof,theorem,condition,lemma,mybox,remark"
