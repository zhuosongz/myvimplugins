" System vimrc file for MacVim
"
" Maintainer:	Bjorn Winckler <bjorn.winckler@gmail.com>
" Last Change:	Sat Aug 29 2009
source $VIMRUNTIME/vimrc_example.vim
source $VIMRUNTIME/mswin.vim
behave mswin

if has("gui_running")
    set encoding=utf-8
    set fileencodings=ucs-bom,utf-8,chinese,prc,taiwan,latin-1

    if has("win32")
        set fileencoding=chinese
    else
        set fileencoding=utf-8
    endif

    let &termencoding=&encoding

source $VIMRUNTIME/delmenu.vim
source $VIMRUNTIME/menu.vim
language messages zh_CN.utf-8
endif

set nu!                    "显示行号
"set wrap!                 "自动折行
"set noautoindent          "不自动对齐
set autoindent             "自动对齐
"全局性地关闭令人讨厌的模式高亮（特别是换行符）：
set hls!
"文档的一行不超过110个字符
match DiffAdd '\%>110v.*'
"设置并开启拼写检查，设置语言为en。
"setlocal spell spelllang=en


set nolinebreak
set number
set so=1
set sidescrolloff=1
set sidescroll=1
set makeprg=make
set tabstop=4
set softtabstop=4
set shiftwidth=4
set fdc=2 
"set fdm=syntax
set fdm=marker "将折叠方式设为marker
syn on
compiler gcc
set guioptions-=T
"set guioptions-=r
"set guioptions-=L
"set guioptions+=i
"set guioptions-=m
"set guioptions+=c
"set! fileformat=unix
"set fileformats=unix
set cindent
set nocompatible
set expandtab
set smarttab
set linespace=1
set foldopen-=search " don't open folds when you search into them
set foldopen-=undo " don't open folds when you undo stuff
set fsync
set laststatus=2
"set statusline=%t%r%h%w\ [%Y]\ [%{&ff}]\ [%{&fenc}:%{&enc}]\ [%08.8L]\ [%p%%-%P]\ [%05.5b]\ [%04.4B]\ [%08.8l]%<\ [%04.4c-%04.4v%04.4V]
set nocursorline
set nocursorcolumn
set updatecount=819222
set undolevels=819222
"set history=819222
set nobackup! "不生成备份文件（~文件）。
set sessionoptions+=unix,slash
set fo=tcqmM
"即formatoptions。设置vim的文本格式，默认是tcq。t表示自动换行（不包括注
"释），c表示注释也自动换行，换行的时候会自动在新行前面补注释符，q表示注
"释可以使用gq命令来格式化。m表示可以在任何值高于255 的多字节字符上分行，
"这对亚洲文本尤其有用。M表示在连接行时，不要在多字节字符之前或之后插入
"空格。
set showmode
"显示工作模式。例如编辑的时候按i，在vim屏幕下方会显示– INSERT –表示你在
"插入状态。 
set ru 
"打开 VIM 的状态栏标尺。它能即时显示当前光标所在位置在文件中的行
"号、列号，以及对应的整个文件的百分比。
set laststatus=2 
"在vim屏幕下方显示状态栏。状态栏会显示文件名称、光标位置等信息。


"以下是编辑器的显示色彩配置
"方案一
"colorscheme carvedwood
"colorscheme slate
"colorscheme ps_color
"colorscheme cool "BEST
"colorscheme InkPot
"colorscheme xoria256
"colorscheme southernlights
syntax enable
set background=light
" colorscheme molokai
set relativenumber

" 关闭警告提示音
set noerrorbells
set vb t_vb=

"方案二
"colorscheme moria
" let moria_style = 'black'
" let moria_style = 'dark'
" colo moria

"方案三
" Color scheme at present. 
" if ! has("gui_running") 
"    set t_Co=256 
" endif 
" set background=light gives a different style, feel free to choose between " them. 
" set background=dark 
" colors peaksea 
" colors jlbcool


"设置latex suite
set shellslash
filetype indent on
filetype plugin on
filetype on
set grepprg=grep\ -nH\ $*
" end

" autocmd Filetype tex source D:\Practical\Vim\vimfiles\auctex.vim

" 设置缩进
set shiftwidth=4
"set tabstop=4


"自动检测文件类型
filetype on


"自动折行（软回车）
"set textwidth=109
"set wrap!

"自动换行（硬回车）
"set textwidth=49 
set nowrap
"set wrapmargin=10

set winaltkeys=no
" set macmeta


"execute pathogen#infect()
" filetype plugin indent on
" "Hello, World!"
" 	let g:loaded_vimballPlugin= 1
" 	let g:loaded_vimball      = 1
":call plug#begin('~/.vim/plugged')

" Make sure you use single quotes
" Add two eggs

"set nocompatible              " be iMproved, required
" filetype off                  " required







set nocompatible

" The default for 'backspace' is very confusing to new users, so change it to a
" more sensible value.  Add "set backspace&" to your ~/.vimrc to reset it.
set backspace+=indent,eol,start

" Disable localized menus for now since only some items are translated (e.g.
" the entire MacVim menu is set up in a nib file which currently only is
" translated to English).
set langmenu=none

set nocompatible              " be iMproved, required
filetype off                  " required

" set the runtime path to include Vundle and initialize
set rtp+=~/Dropbox/config/vim/.vim/bundle/Vundle.vim
call vundle#begin('~/Dropbox/config/vim/.vim/bundle')
" alternatively, pass a path where Vundle should install plugins
"call vundle#begin('~/some/path/here')

" let Vundle manage Vundle, required
Plugin 'VundleVim/Vundle.vim'

"
"
"
" Plugins 
"Plugin 'Vimball'
Plugin 'scrooloose/nerdtree'
Plugin 'jalvesaq/Nvim-R'

" open a NERDTree automatically when vim starts up if no files were specified
"autocmd StdinReadPre * let s:std_in=1
"autocmd VimEnter * if argc() == 0 && !exists("s:std_in") | NERDTree | endif





" Stick this in your vimrc to open NERDTree with Ctrl+n (you can set whatever
" key you want):
"map <C-g> :NERDTreeToggle<CR>



" close vim if the only window left open is a NERDTree 
" Stick this in your vimrc:
autocmd bufenter * if (winnr("$") == 1 && exists("b:NERDTree") && b:NERDTree.isTabTree()) | q | endif

" Use these variables in your vimrc. Note that below are default arrow
" symbols
let g:NERDTreeDirArrowExpandable = '▸'
let g:NERDTreeDirArrowCollapsible = '▾'

Plugin 'kana/vim-textobj-user'
Plugin 'rbonvall/vim-textobj-latex'
Plugin 'octol/vim-cpp-enhanced-highlight'
Plugin 'tpope/vim-surround'

" Plugin 'kien/ctrlp.vim'
Plugin 'kien/ctrlp.vim'

"Change the default mapping and the default command to invoke CtrlP:
  let g:ctrlp_map = '<C-p>'
  let g:ctrlp_cmd = 'CtrlP'
  let g:ctrlp_user_command = 'find %s -type f'        " MacOSX/Linux

" Plugin 'altercation/vim-colors-solarized'
Plugin 'altercation/vim-colors-solarized'
syntax enable
set background=dark
colorscheme solarized

" Airlines
Plugin 'vim-airline/vim-airline'
Plugin 'vim-airline/vim-airline-themes'
"Plugin 'ybian/smartim'
Plugin 'machakann/vim-sandwich'

"Plugin 'xuhdev/vim-latex-live-preview'
" add the following to your vimrc to enable the extension:
let g:airline#extensions#tabline#enabled = 1

" Separators can be configured independently for the tabline, so here is how
" you can define "straight" tabs:
let g:airline#extensions#tabline#left_sep = ' '
let g:airline#extensions#tabline#left_alt_sep = '|'

" YouCompleteMe
" 
" Plugin 'valloric/youcompleteme'

" Plugin 'justinmk/vim-sneak'
"let g:sneak#label = 1


" Unisnips
" 
" Track the engine.
Plugin 'SirVer/ultisnips'

" Snippets are separated from the engine. Add this if you want them:
Plugin 'honza/vim-snippets'

" Trigger configuration. Do not use <tab> if you use https://github.com/Valloric/YouCompleteMe.
let g:UltiSnipsExpandTrigger=",a"
let g:UltiSnipsJumpForwardTrigger=",b"
let g:UltiSnipsJumpBackwardTrigger=",z"




" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

Plugin 'lervag/vimtex'
Plugin 'gerw/vim-latex-suite'

set winaltkeys=no


" easy align 
"
Plugin 'junegunn/vim-easy-align'
" Start interactive EasyAlign in visual mode (e.g. vipga)

xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)

" Plugin 'tpope/vim-fugitive'

" Plugin 'itchyny/calendar.vim'
" let g:calendar_diary='$HOME/Dropbox/Diary'

"Plugin 'itchyny/calendar.vim'
"let g:calendar_google_calendar = 1
"let g:calendar_google_task = 1
"
"
"
"Plugin 'vimtips.zip'
"
"Plugin 'luochen1990/rainbow'
"let g:rainbow_active = 1 "0 if you want to enable it later via :RainbowToggle
"
"Plugin 'w0rp/ale'
"Plugin 'vim-pandoc/vim-pandoc'
"Plugin 'vim-pandoc/vim-pandoc-syntax' 
"Plugin 'vim-pandoc/vim-rmarkdown'
Plugin 'easymotion/vim-easymotion'

" map f <Leader> 

" map <Leader> <Plug>(easymotion-prefix)
" <Leader>f{char} to move to {char}
map  <Leader>f <Plug>(easymotion-bd-f)
nmap <Leader>f <Plug>(easymotion-overwin-f)

" s{char}{char} to move to {char}{char}
nmap ss <Plug>(easymotion-overwin-f2)

" Move to line
map <Leader>L <Plug>(easymotion-bd-jk)
nmap <Leader>L <Plug>(easymotion-overwin-line)

" Move to word
map  <Leader>w <Plug>(easymotion-bd-w)
nmap <Leader>w <Plug>(easymotion-overwin-w)
"""
"
"
"
"Plugin 'xolox/vim-misc'
"Plugin 'xolox/vim-notes'
""let g:notes_suffix = '.tex'
"
" Plugin 'vimoutliner/vimoutliner'
"
"Plugin 'matchit.zip'
"
"let b:match_words = 'if:end if'  "'\<big\>:\<big\>'
""let maplocalleader = '\'  " # Default if option is not set
"
"let g:notes_directories = ['~/Dropbox/Shared Notes']
"
Plugin 'tpope/vim-repeat'
" Plugin 'vim-scripts/Vim-R-plugin'

" Plugin 'klen/python-mode'
"
"
"
"Plugin 'tpope/tpope-vim-abolish'
"
Plugin 'tpope/vim-commentary'
Plugin 'kien/rainbow_parentheses.vim'
"
"
"
"Plugin 'tpope/vim-speeddating'
"
""Plugin 'svermeulen/vim-easyclip'
""set clipboard=unnamed
"
"

" Plugin 'jceb/vim-orgmode'
Plugin 'MatlabFilesEdition'
" Plugin 'matlab.vim'
" Plugi 'daeyun/vim-matlab'

" Plugin 'vimwiki/vimwiki'
" Plugin 'suan/vim-instant-markdown'

" vimwiki stuff "
" Run multiple wikis "
" let g:vimwiki_list = [{'path': '~/Dropbox/vimwiki/'} ]

" let g:vimwiki_list = [{
"   \ 'path': '$HOME/Dropbox/vimwiki/',
"   \ 'template_path': '$HOME/Dropbox/vimwiki/templates',
"   \ 'template_default': 'default',
"   \ 'template_ext': '.html'}]

"  \ 'syntax': 'markdown',
"  \ 'ext': '.md'}]
" vimwiki - Personal Wiki for Vim
" https://github.com/vimwiki/vimwiki
" set nocompatible
" filetype plugin on
" syntax on
" vimwiki with markdown support
" let g:vimwiki_ext2syntax = {'.md': 'markdown', '.markdown': 'markdown', '.mdown': 'markdown'}
" helppage -> :h vimwiki-syntax 

" vim-instant-markdown - Instant Markdown previews from Vim
" https://github.com/suan/vim-instant-markdown

" let g:instant_markdown_autostart = 0	"disable autostart
" map <leader>md :InstantMarkdownPreview<CR>


" map <A-Space> <Plug>VimwikiToggleListItem
" let g:vimwiki_table_mappings = 0
" let g:vimwiki_global_ext = 0

" au BufRead,BufNewFile *.wiki set filetype=vimwiki
" :autocmd FileType vimwiki map d :VimwikiMakeDiaryNote
" function! ToggleCalendar()
"   execute ":Calendar"
"   if exists("g:calendar_open")
"     if g:calendar_open == 1
"       execute "q"
"       unlet g:calendar_open
"     else
"       g:calendar_open = 1
"     end
"   else
"     let g:calendar_open = 1
"   end
" endfunction
" :autocmd FileType vimwiki map c :call ToggleCalendar()
Plugin 'jiangmiao/auto-pairs'
" Plugin 'vim-pandoc/vim-pandoc'
" Plugin 'vim-pandoc/vim-pandoc-syntax'
" Plugin 'vim-pandoc/vim-rmarkdown'



Plugin 'shougo/neocomplete.vim'
" Plugin 'tpope/vim-abolish'
" Plugin 'kakkyz81/evervim'
"Bundle 'glidenote/newdayone.vim'
" ==================================================
"   Settings for neocomplete
" ==================================================
"Note: This option must be set in .vimrc(_vimrc).  NOT IN .gvimrc(_gvimrc)!
" Disable AutoComplPop.
let g:acp_enableAtStartup = 0
" Use neocomplete.
let g:neocomplete#enable_at_startup = 1
" Use smartcase.
let g:neocomplete#enable_smart_case = 1
" Set minimum syntax keyword length.
let g:neocomplete#sources#syntax#min_keyword_length = 3

" Define dictionary.
let g:neocomplete#sources#dictionary#dictionaries = {
    \ 'default' : '',
    \ 'vimshell' : $HOME.'/.vimshell_hist',
    \ 'scheme' : $HOME.'/.gosh_completions'
        \ }

" Define keyword.
if !exists('g:neocomplete#keyword_patterns')
    let g:neocomplete#keyword_patterns = {}
endif
let g:neocomplete#keyword_patterns['default'] = '\h\w*'

" Plugin key-mappings.
inoremap <expr><C-g>     neocomplete#undo_completion()
inoremap <expr><C-l>     neocomplete#complete_common_string()

" Recommended key-mappings.
" <CR>: close popup and save indent.
inoremap <silent> <CR> <C-r>=<SID>my_cr_function()<CR>
function! s:my_cr_function()
  return (pumvisible() ? "\<C-y>" : "" ) . "\<CR>"
  " For no inserting <CR> key.
  "return pumvisible() ? "\<C-y>" : "\<CR>"
endfunction
" <TAB>: completion.
inoremap <expr><TAB>  pumvisible() ? "\<C-n>" : "\<TAB>"
" <C-h>, <BS>: close popup and delete backword char.
inoremap <expr><C-h> neocomplete#smart_close_popup()."\<C-h>"
inoremap <expr><BS> neocomplete#smart_close_popup()."\<C-h>"

" ==================================================
"   End of setting of neocomplete
" ==================================================


" Close popup by <Space>.
"inoremap <expr><Space> pumvisible() ? "\<C-y>" : "\<Space>"

" AutoComplPop like behavior.
"let g:neocomplete#enable_auto_select = 1

" Shell like behavior(not recommended).
"set completeopt+=longest
"let g:neocomplete#enable_auto_select = 1
"let g:neocomplete#disable_auto_complete = 1
"inoremap <expr><TAB>  pumvisible() ? "\<Down>" : "\<C-x>\<C-u>"

" Enable omni completion.
autocmd FileType css setlocal omnifunc=csscomplete#CompleteCSS
autocmd FileType html,markdown setlocal omnifunc=htmlcomplete#CompleteTags
autocmd FileType javascript setlocal omnifunc=javascriptcomplete#CompleteJS
autocmd FileType python setlocal omnifunc=pythoncomplete#Complete
autocmd FileType xml setlocal omnifunc=xmlcomplete#CompleteTags

" Enable heavy omni completion.
if !exists('g:neocomplete#sources#omni#input_patterns')
  let g:neocomplete#sources#omni#input_patterns = {}
endif
"let g:neocomplete#sources#omni#input_patterns.php = '[^. \t]->\h\w*\|\h\w*::'
"let g:neocomplete#sources#omni#input_patterns.c = '[^.[:digit:] *\t]\%(\.\|->\)'
"let g:neocomplete#sources#omni#input_patterns.cpp = '[^.[:digit:] *\t]\%(\.\|->\)\|\h\w*::'

" For perlomni.vim setting.
" https://github.com/c9s/perlomni.vim
let g:neocomplete#sources#omni#input_patterns.perl = '\h\w*->\h\w*\|\h\w*::'


filetype plugin indent on
call vundle#end()            " required
filetype plugin indent on    " required
set wrap 
" To ignore plugin indent changes, instead use:
"filetype plugin on
"
" Brief help
" :PluginList       - lists configured plugins
" :PluginInstall    - installs plugins; append `!` to update or just :PluginUpdate
" :PluginSearch foo - searches for foo; append `!` to refresh local cache
" :PluginClean      - confirms removal of unused plugins; append `!` to auto-approve removal
"
" see :h vundle for more details or wiki for FAQ
" Put your non-Plugin stuff after this line

if has("user_commands")
    command! -bang -nargs=? -complete=file E e<bang> <args>
    command! -bang -nargs=? -complete=file W w<bang> <args>
    command! -bang -nargs=? -complete=file Wq wq<bang> <args>
    command! -bang -nargs=? -complete=file WQ wq<bang> <args>
    command! -bang Wa wa<bang>
    command! -bang WA wa<bang>
    command! -bang Q q<bang>
    command! -bang QA qa<bang>
    command! -bang Qa qa<bang>
endif



syntax on
let g:loaded_vimballPlugin = 1
"so '~/.vim/bundle/Vimball/autoload/vimballPlugin.vim'

augroup nonvim
   au!
   au BufRead *.png,*.jpg,*.pdf,*.gif,*.xls*,*.ppt*,*.doc*,*.rtf sil exe "!open " . shellescape(expand("%:p")) | bd | let &ft=&ft
augroup end

" autocmd BufNewFile,BufReadPost *.md set filetype=tex
"
" type in the date
" imap <F6> <C-R>=strftime("%Y-%m-%d")<CR>
"set updatetime=1000
"autocmd CursorHoldI * silent w
