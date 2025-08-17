" this file may contain utf-8 characters
scriptencoding utf-8

"---------------------------
" THINGS THAT AFFECT VISUALS

" turn on syntax highlighting
syntax enable
set linebreak " causes vim to wrap at word boundaries.
              " terrible name
set tw=70     " causes vim to wrap at 70 characters
              " Yeah, yeah. I know. My eyes aren't what
              " they used to be and my fonts are bigger.

" and spelling
" uncomment set spell to turn it on by default
" set spell
set spelllang=en_us
" specify a color scheme
silent! colorscheme inori_m
" set the font for GUI vims
" https://fonts.google.com/specimen/Roboto+Mono
" set guifont=RobotoMono-Regular:h20
" set guifont=Roboto\ Mono\ for\ Powerline:h20
set guifont=JetBrains\ Mono\ Medium:h20

" display hidden characters
set list
" note without "list" you can't
" move the cursor to start of any line that
" starts with a tab. - No clue why.
set listchars=tab:»_,trail:·

" terminal vim's cursor is a block in insert and
" normal mode, which makes it hard to tell what mode
" you're in when quickly switching (without having
" stop and read the status line)
" found here: https://stackoverflow.com/a/42118416/13973
if has('gui_running') == 0
  let &t_SI = "\e[5 q"
  " note: for me (iterm2 on mac)
  " the blinking options just produce their steady variants
  let &t_EI = "\e[2 q"
    " Ps = 0  -> blinking block.
    " Ps = 1  -> blinking block (default).
    " Ps = 2  -> steady block.
    " Ps = 3  -> blinking underline.
    " Ps = 4  -> steady underline.
    " Ps = 5  -> blinking bar (xterm).
    " Ps = 6  -> steady bar (xterm).

  " optional reset cursor on start:
  augroup terminalCursorCorrection
  au!
  autocmd VimEnter * silent !echo -ne "\e[2 q"
  autocmd VimEnter * redraw!
  augroup END
end


set conceallevel=0

" make comments display in italic _if_ your
" chosen font supports that
" highlight Comment cterm=italic gui=italic
highlight Comment gui=italic

" the terminal needs more scrollback? Increase this number
" set termwinscroll=10000

"---------------
" CROSSHAIRS
" set up somecrosshairs to help you find the cursor
hi cursorline   cterm=none ctermbg=black guibg=#303030
hi cursorcolumn cterm=none ctermbg=black guibg=#303030
set cursorcolumn
set cursorline
"----------------
" THE WARNING BOUNDARY
hi ColorColumn ctermbg=0 guibg=#081c23
" this is also used by the diminactive plugin
" set colorcolumn=80
" make a vertical column in the background at 80 characters

"----------------
" THE VERTICAL SPLIT DIVIDERS
" set the character to a space
set fillchars=stl:^,stlnc:=,vert:\ ,fold:-,diff:-
" make it orange!
highlight VertSplit guibg=Orange guifg=Black ctermbg=6 ctermfg=0
"

" the following depends on the vim-show-marks plugin
" and causes the plugin to show / refresh the gutter with your marks.
nnoremap <leader>m :DoShowMark<cr>



"-------------------------------------------
" tabs vs spaces!
" let tabs be tabs!

set noexpandtab
" gives you tab if you type a tab rather than converting it
" to something else. See also TabToggle

set softtabstop=0
" fuck softtabs
" see TabToggle function below

set tabstop=4
" tabs display as four spaces

set shiftwidth=4
" shorter version of the above is :set noet ci pi sts=0 sw=4 ts=4

"------------------------------------------------
" SPLIT CONFIGURATION
set splitright " new vertical splits are on the right
set splitbelow " new horizontal splits are on the bottom
"------------------------------------------------
" FOLD CONFIGURATION
" makes your folding persist across sessions
autocmd BufWinLeave *.* mkview " store folds when you leave a buffer
autocmd BufWinEnter *.* silent loadview "load saved folds when load buffer
"------------------------------------------------
" CHANGE NETRW look
"nerdtree without nerdtree
let g:netrw_banner       = 0  " removes the netrw banner
let g:netrw_liststyle    = 3  " tree style listing
let g:netrw_browse_split = 4  " see help on this one. lots of useful options
let g:netrw_altv         = 1  " change from left splitting to right splitting
let g:netrw_winsize      = 25 " initial size of new explore windows

" By default netrw leaves unmodified buffers open. This autocommand
" deletes them when they're hidden (using :q for example).
autocmd FileType netrw setl bufhidden=delete
" uncommenting the following will cause the
" file browser to open automatically when you launch vim
" augroup ProjectDrawer
"   autocmd!
"   autocmd VimEnter * :Vexplore
" augroup END




"------------------------------------------------
" LINE NUMBERS
" give line numbers a black background
highlight LineNr ctermbg=0 guibg=Black
" display them by default
set number

"------------------------------------------------
" Enable the toggling of relative or
" absolute number mode.
function! g:ToggleNuMode()
	if(&rnu == 1)
		set nornu
		set nu
	else
		set rnu
	endif
endfunc
nnoremap <C-L> :call g:ToggleNuMode()<cr>
"------




"------------------------------------------------
" NAVIGATION MODIFICATIONS
" There are several commands which move the cursor within the line.
" When you get to the start/end of a line then these commands will
" fail as you cannot go on. However, many users expect the cursor to
" be moved onto the previous/next line. Vim allows you to chose which
" commands will "wrap" the cursor around the line borders. Here I allow
" the cursor left/right keys as well as the 'h' and 'l' command to do that.

set whichwrap+=<,>,h,l


"------------------------------------------------
" Modify How Indentation Works
" the following lets you keep indenting or outdenting without reselecting
vnoremap < <gv
vnoremap > >gv

" Abbreviations
:iab ferr (format (current-error-port) "XXX~%")<LEFT><LEFT><LEFT><LEFT>

"-----------------------------------------------
" set working directory to git project root
" or directory of current file if not git project
function! SetProjectRoot()
  " default to the current file's directory
  lcd %:p:h
  let git_dir = system("git rev-parse --show-toplevel")
  " See if the command output starts with 'fatal' (if it does, not in a git repo)
  let is_not_git_dir = matchstr(git_dir, '^fatal:.*')
  " if git project, change local directory to git project root
  if empty(is_not_git_dir)
    lcd `=git_dir`
  endif
endfunction

:cabbr spr :call SetProjectRoot()



" RANDOM STUFF
"------------------------------------------------
" store temp files in a central spot
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
" it'll use the first one it finds
" instead of polluting your working directory with
" temp files
"

"------------------------------------------------
" have rubocop autocorrect ruby files
" autocmd BufWritePost *.rb silent !rubocop -a <afile> 2>&1 &
"------------------------------------------------
" helps you jump to the next conflict divider
:cabbr conflicted /<<<<<<<\\|>>>>>>>\\|=======
:cabbr fuck /<<<<<<<\\|>>>>>>>\\|=======

"------------------------------------------------
" because the real quickfix window commands are non-intuitive
:cabbr qf cw   " open quickfix (window)
:cabbr cqf ccl " close quickfix (window)
:cabbr qqf ccl " quit quickfix (window)
"------------------------------------------------
" Ruby convert a line from old hash syntax (rocket) to new (colon)
function! g:NewHashSyntax()
	:'<,'>s/:\(\S\{-}\) => /\1: /g
endfunction
:cabbr nhs :call NewHashSyntax()

"------------------------------------------------
" Does hitting the tab key result in
" tab charaters or space characters being
" inserted into your file. This function will toggle it.
function TabToggle()
	if &expandtab
		set shiftwidth=4
		set tabstop=4
		set softtabstop=0
		set noexpandtab
	else
		" damn rails geeks and their 2 spaces
		set shiftwidth=2
		set softtabstop=2
		set expandtab
	endif
endfunction
:cabbr tt :call TabToggle()

" movement over display lines when wrapping
" https://vim.fandom.com/wiki/Move_cursor_by_display_lines_when_wrapping
" noremap <silent> <Leader>w :call ToggleWrap()<CR>
function ToggleWrap()
  if &wrap
    echo "Wrap OFF"
    setlocal nowrap
    set virtualedit=all
    silent! nunmap <buffer> <Up>
    silent! nunmap <buffer> <Down>
    silent! nunmap <buffer> <Home>
    silent! nunmap <buffer> <End>
    silent! iunmap <buffer> <Up>
    silent! iunmap <buffer> <Down>
    silent! iunmap <buffer> <Home>
    silent! iunmap <buffer> <End>
  else
    echo "Wrap ON"
    setlocal wrap linebreak nolist
    set virtualedit=
    setlocal display+=lastline
    noremap  <buffer> <silent> <Up>   gk
    noremap  <buffer> <silent> <Down> gj
    noremap  <buffer> <silent> <Home> g<Home>
    noremap  <buffer> <silent> <End>  g<End>
    inoremap <buffer> <silent> <Up>   <C-o>gk
    inoremap <buffer> <silent> <Down> <C-o>gj
    inoremap <buffer> <silent> <Home> <C-o>g<Home>
    inoremap <buffer> <silent> <End>  <C-o>g<End>
  endif
endfunction
:cabbr twrap :call ToggleWrap()

"
"------------------------------------------------
" ENABLE DIRECTORY SPECIFIC .vimrc FILES
" (project specific tweaks can be very useful)
set exrc " Enable use of directory-specific .vimrc
set secure " Only run autocommands owned by me


" SEARCH MODIFICATIONS
" See also "vim-side-search" in plugins (and its config)
" semakes /~style searches case sensitive only if there is a capital letter in
" the search expression. *~style searches will continue to be consistently
" case-sensitive
set ignorecase
set smartcase
set scrolloff=3

" highlight search terms...
set hlsearch
set incsearch " ... dynamically as they are typed
" if you type a space during search it will enter .*
" which makes it into a fuzzy search.
" via 7sidedmarble: https://www.reddit.com/r/vim/comments/ejiy8s/dead_simple_fuzzy_searching_with/
" cnoremap <expr> <space> '/?' =~ getcmdtype() ? ".*" : "<space>"

" in insert mode backspace will sometimes NOT delete past a certain point
" this fixes it
" see help bs
set backspace=indent,eol,start
" shorter method of above is : set bs=2

"------------------------------
"MacVim / GVim tweaks
if has('gui_running')
	set encoding=utf8

	" MACVIM
	" In MacVim, you can have multiple tabs open. This mapping makes Ctrl-Tab
	" switch between them, like browser tabs. Ctrl-Shift-Tab goes the other way.
	noremap <C-Tab> :tabnext<CR>
	noremap <C-S-Tab> :tabprev<CR>

	" Switch to specific tab numbers with Command-number
	noremap <D-1> :tabn 1<CR>
	noremap <D-2> :tabn 2<CR>
	noremap <D-3> :tabn 3<CR>
	noremap <D-4> :tabn 4<CR>
	noremap <D-5> :tabn 5<CR>
	noremap <D-6> :tabn 6<CR>
	noremap <D-7> :tabn 7<CR>
	noremap <D-8> :tabn 8<CR>
	noremap <D-9> :tabn 9<CR>
	" Command-0 goes to the last tab
	noremap <D-0> :tablast<CR>
endif


" redirect the output of a Vim or external command into a scratch buffer
" found here: https://vi.stackexchange.com/a/16607/6838
function! Redir(cmd)
  if a:cmd =~ '^!'
    execute "let output = system('" . substitute(a:cmd, '^!', '', '') . "')"
  else
    redir => output
    execute a:cmd
    redir END
  endif
  tabnew
  setlocal nobuflisted buftype=nofile bufhidden=wipe noswapfile
  call setline(1, split(output, "\n"))
  put! = a:cmd
  put = '----'
endfunction
command! -nargs=1 Redir silent call Redir(<f-args>)


"------------------------
" html_encode_decode
:cabbr hencode :call HTMLEncode()
:cabbr hdecode :call HTMLDecode()
:cabbr hstrip :%s/<\_.\{-1,\}>//g

"------------------------
" command line formatter
command! -range FormatShellCmd <line1>!format_shell_cmd

"------------------------
" Text Expansions
iab sterr $stderr.puts("XXX")<ESC>bi
iab ferr (format (current-error-port) "XXX~%")<ESC>3hi
iab byeb require 'byebug'; byebug

:cabbr vterm :vertical terminal
:cabbr hterm :terminal

" PLUGINS ---------------
"------------------------
"------------------------
"------------------------
"------------------------
"------------------------
"------------------------
"------------------------
"------------------------
"------------------------
" PLUGINS
if empty(glob('~/.vim/autoload/plug.vim'))
  silent !curl -fLo ~/.vim/autoload/plug.vim --create-dirs
    \ https://raw.githubusercontent.com/junegunn/vim-plug/master/plug.vim
  autocmd VimEnter * PlugInstall --sync | source $MYVIMRC
endif
" IF this directory doesn't have a .vimrc
" OR if this is the home directory and thus the .vimrc in question is this one
if ! filereadable(".vimrc") || len(split(getcwd(), "/")) == 2
	" if there isn't a local .vimrc
	" then load these
	" if there IS it will be read, and
	" do... whatever

	call plug#begin('~/.vim/plugged-coding')
	Plug 'tomtom/tcomment_vim'
	Plug 'bling/vim-airline'
	" tweak tabline display to be more useful
	" suggests changes to your color theme too
	Plug 'yfiua/tabline.vim'
	Plug 'terryma/vim-expand-region'
	Plug 'tpope/vim-surround'
	" Surround.vim is "all about "surroundings": parentheses, brackets,
	" quotes, XML tags, and more. The plugin provides mappings to easily
	" delete, change and add such surroundings in pairs.
	Plug 'godlygeek/tabular'
	"vvv indentline shows levels of indentation visually
	Plug 'Yggdroot/indentLine'
	let g:indentLine_char = '|'
	Plug 'masukomi/rainbow_parentheses.vim'

	" vvv gives you <leader>be for better buffer exploration
	Plug 'jlanzarotta/bufexplorer'

	"vvv dims inactive splits
	Plug 'blueyed/vim-diminactive'
	"let g:diminactive_use_syntax = 1
	" let g:diminactive_enable_focus = 1

	"vvv shows marks in the gutter
	Plug 'jacquesbh/vim-showmarks'
	Plug 'vim-scripts/ShowMarks'


    " The Toolkit for Vim Color Scheme Designers!
	Plug 'lifepillar/vim-colortemplate'

	" Plug 'nestorsalceda/vim-strip-trailing-whitespaces'
	Plug 'masukomi/vim-strip-trailing-whitespaces'
	"-------------------
	" MISC EXTERNAL APPS
	" Plug 'blindFS/vim-taskwarrior'
	" if you have fzf installed on your system already
	" Plug '/usr/local/opt/fzf'
	" Plug 'junegunn/fzf.vim'
	" else...
	Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
	Plug 'mileszs/ack.vim'
	Plug 'ddrscott/vim-side-search'
	Plug 'tpope/vim-fugitive'
    Plug 'tpope/vim-rhubarb' " If fugitive.vim is the Git, rhubarb.vim is the Hub. - Tim Pope
	Plug 'segeljakt/vim-silicon' " create pretty code screenshots

    " Plug '~/workspace/private_comments/vim_private_comments'
    Plug 'masukomi/vim_private_comments'

	"-------------------
	" LANGUAGE PLUGINS...
	"
	Plug 'elixir-editors/vim-elixir'
	Plug 'rhysd/vim-crystal', { 'for': 'crystal' }
	" Plug 'ElmCast/elm-vim', { 'for': 'elm' }
	Plug 'dag/vim-fish', { 'for': 'fish' }
	Plug 'fatih/vim-go', { 'for': 'go' }
	Plug 'Raku/vim-raku', { 'for': 'raku' }
	" Plug 'myitcv/govim', { 'for': 'go' }
	" let g:go_template_autocreate=0

	" config tweaks for vim-go and go coding
	" can be found in ~/.vim/ftplugin/go.vim

	"Plug 'myitcv/govim', { 'for': 'go' }
	" govim has a number of config tweaks below

	" Plug 'nono/vim-handlebars', { 'for': 'handlebars' }
	Plug 'mustache/vim-mustache-handlebars'
	" Plug 'jelera/vim-javascript-syntax', { 'for': 'javascript' }
	Plug 'vim-ruby/vim-ruby', { 'for': 'ruby' }
	" Plug 'tpope/vim-endwise'
	Plug 'rust-lang/rust.vim', { 'for': 'rust' }
	" Plug 'pedrohdz/vim-yaml-folds', { 'for': 'yaml' }
	" Plug 'kballard/vim-swift', { 'for': 'swift' }
	"Plug 'Keithbsmiley/swift.vim'
	"Plug 'kchmck/vim-coffee-script'
	"""Plug 'w0rp/ale' is now renamed dense-analysis/ale
	Plug 'dense-analysis/ale' " a linter for many languages
	"Plug 'vim-scripts/paredit.vim'
	" join multiline statements into one gJ
	" split single line statements into multi-line gS
	Plug 'AndrewRadev/splitjoin.vim'
	" racket / scheme / lisp
	" Plug 'bhurlow/vim-parinfer'
	Plug 'wlangstroth/vim-racket'
	" Plug 'ds26gte/scmindent'

	Plug 'dylon/vim-antlr'


	"-------------------
	" MARKDOWN PLUGINS
	Plug 'clarke/vim-renumber', { 'for': 'markdown' }
	" Plug 'Scuilion/markdown-drawer', { 'for': 'markdown' }
    "
	Plug 'masukomi/vim-markdown-folding', { 'for': 'markdown' }
	" Plug 'file:///Users/masukomi/workspace/vim-markdown-folding'

	Plug 'masukomi/html_encode_decode' " html  encoder decoder
"
	Plug 'itspriddle/vim-marked', { 'for': 'markdown' }
" 	" OS X Only - Open the current Markdown buffer in Marked. Supports Marked 1 and 2.
" 	" Marked: http://marked2app.com/
" 	" END MARKDOWN PLUGINS
" 	"-------------------
"
" 	"
	" ----------------------
	" CTAGS
	" ctags generated via an external app
	" historically this was Exuberant Ctags but the project
	" has been abandoned. The currently maintained fork of
	" it is called Universal Ctags and can be found here
	" https://ctags.io/
	"
	" To see what programming languages are supported
	" run ctags --list-languages in your terminal.
	"
	" some (many?) of the language specific vim plugins
	" (like swift.vim) add files that tell ctags how
	" to process new languages
	"
	" gutentags will regenerate your tag file on save
	" https://bolt80.com/gutentags/
	" Plug 'ludovicchabant/vim-gutentags'
	" Tim Pope's Effortless ctags post explains how to
	" leverage git hooks to make sure new files and cloned
	" repos get indexed by ctags without you needing to
	" edit them.
	" https://tbaggery.com/2011/08/08/effortless-ctags-with-git.html
	" a simpler alternative to vim-gutentags
	"Plug 'craigemery/vim-autotag'
	"or
	"Plug 'xolox/vim-easytags'
	" vim-easytags appears to not be receiving a great level of
	" maintenance.
	"
	" Once you _have_ the ctags created, and maintained,
	" you want to do something useful with them.
	"
	" Tagbar
	" Tagbar is a Vim plugin that provides an easy way to browse
	" the tags of the current file and get an overview of its
	" structure. It does this by creating a sidebar that
	" displays the ctags-generated tags of the current file,
	" ordered by their scope. This means that for example
	" methods in C++ are displayed under the class they are
	" defined in.
	" http://majutsushi.github.io/tagbar/
	" Plug 'majutsushi/tagbar'
	" causes f8 key to toggle Tagbar sidebar
	" nmap <F8> :TagbarToggle<CR>

	" Autocompletion!
	" * youcompleteme - requires Python 2
	"                 - may not work with multibyte characters
	"                 - has fuzzycompletion
	" * neocomplete
	" * supertab
	"   - https://github.com/ervandew/supertab
	"   - https://www.vim.org/scripts/script.php?script_id=1643
	"   - "Supertab is a vim plugin which allows you to
	"     use <Tab> for all your insert completion needs
	"     (:help ins-completion)"
	"   - seems similar to VimCompletesMe
	" * VimCompletesMe
	"   - https://github.com/ajh17/VimCompletesMe
	"   - "A super simple, super minimal, super
	"     light-weight tab completion plugin for Vim. "
	"   - no external dependencies
	" * deoplete
	" Plug 'Shougo/deoplete.nvim'
	" if has('nvim')
	"   Plug 'Shougo/deoplete.nvim', { 'do': ':UpdateRemotePlugins' }
	" else
	"   Plug 'Shougo/deoplete.nvim'
	"   Plug 'roxma/nvim-yarp'
	"   Plug 'roxma/vim-hug-neovim-rpc'
	" endif
	let g:deoplete#enable_at_startup = 1
	"   - https://github.com/Shougo/deoplete.nvim
	"   - provides an extensible and asynchronous
	"     completion framework for neovim/Vim8.
	"   - requires modern (neo)vim compiled with
	"     python3 support
	" END PLUGINS
	"-------------------




	call plug#end()




	"-----------
	" PlUGIN SPECIFIC STUFF
	"
	"-----------
	"strip-trailing-whitespace
	"exclude markdown files because it' actually significant
	autocmd FileType markdown let b:noStripWhitespace=1

	"-----------
	" w0rp/ale
	" integrate it with Airline
	let g:airline#extensions#ale#enabled = 1
	let g:ale_echo_msg_error_str = 'E'
	let g:ale_echo_msg_warning_str = 'W'
	let g:ale_echo_msg_format = '[%linter%][%severity%] %s '
	" Ack.vim should use the_silver_searcher
	let g:ackprg = 'ag --nogroup --nocolor --column'
	let g:ale_set_quickfix = 0
	let g:ale_open_list = 0

	"-----------
	" vim-side-search
	" How should we execute the search?
	" --heading and --stats are required!
	let g:side_search_prg = 'ag --word-regexp'
	  \. " --ignore='*.js.map'"
	  \. " --heading --stats -B 1 -A 4"

	" Can use `vnew` or `new`
	let g:side_search_splitter = 'vnew'

	" I like 40% splits, change it if you don't
	let g:side_search_split_pct = 0.4

	" SideSearch current word and return to original window
	nnoremap <Leader>ss :SideSearch <C-r><C-w><CR> | wincmd p

	" Create an shorter `SS` command
	command! -complete=file -nargs=+ SS execute 'SideSearch <args>'

	" or command abbreviation
	cabbrev SS SideSearch


	"-----------
	" fzf
	" fuzzy search buffers
	" https://jesseleite.com/posts/2/its-dangerous-to-vim-alone-take-fzf
	nmap <Leader>f :FZF<CR>
	nmap <Leader>b :Buffers<CR>
	" fuzzy search buffer history
	nmap <Leader>h :History<CR>
	" fuzzy search tags in current buffer
	nmap <Leader>t :BTags<CR>
	" fuzzy search tags in ALL buffers
	nmap <Leader>T :Tags<CR>
	" fuzzy search lines in current buffer
	nmap <Leader>l :BLines<CR>
	" fuzzy search lines in ALL buffers
	nmap <Leader>L :Lines<CR>
	" fuzzy search marks
	nmap <Leader>' :Marks<CR>
	" fuzzy search Help docs
	nmap <Leader>H :Helptags!<CR>
	" fuzzy search against existing key mappings
	nmap <Leader>M :Maps<CR>


	" -----------------
	autocmd FileType ruby,eruby let g:rubycomplete_buffer_loading = 1
	autocmd FileType ruby,eruby let g:rubycomplete_classes_in_global = 1
	autocmd FileType ruby,eruby let g:rubycomplete_rails = 1

	" ds26gte/scmindent
	" autocmd BufEnter *.rkt,*.scm :setlocal filetype=scheme
	" autocmd filetype lisp,scheme setlocal equalprg=scmindent.rkt
	"-------------------
	" masukomi/rainbow_parentheses.vim
	augroup RainbowParentheses
		au!
		au VimEnter * RainbowParenthesesToggle
		au Syntax * RainbowParenthesesLoadRound
		au Syntax * RainbowParenthesesLoadSquare
		au Syntax * RainbowParenthesesLoadBraces
	augroup END


	"-------------------
	" masukomi/vim_private_comments
	:cabbr pcv :call private_comments#View()
	:cabbr pcj :call private_comments#JumpToCommentLine()
	:cabbr pcr :call private_comments#RecordComment()
	:cabbr pcd :call private_comments#DeleteComment()
	:cabbr pcc :call private_comments#ClosePCWindow()

	"-------------------
	" dylon/vim-antlr
	au BufRead,BufNewFile *.g set filetype=antlr3
	au BufRead,BufNewFile *.g4 set filetype=antlr4
"
"
" 	"-------------------
" 	" govim
" 	" This represents the minimal .vimrc needed to work with govim
" 	" It is used as part of the automated tests for govim and so will
" 	" always be current
" 	"
" 	" set nocompatible
" 	" set nobackup
" 	" set nowritebackup
" 	" set noswapfile
" 	"
" 	" filetype plugin on
" 	"
" 	" set mouse=a
"
" 	" To get hover working in the terminal we need to set ttymouse. See
" 	"
" 	" :help ttymouse
" 	"
" 	" for the appropriate setting for your terminal. Note that despite the
" 	" automated tests using xterm as the terminal, a setting of ttymouse=xterm
" 	" set ttymouse=sgr
"
" 	"-------------------
" 	" markdown-drawer
" 	nnoremap <Leader>md :MarkDrawer<cr>
"
"
	"-------------------
	" vim-expand-region
	" builds on Plugin 'terryma/vim-expand-region'
	" for expanding visual selections
	 vmap v <Plug>(expand_region_expand)
	 vmap <S-v> <Plug>(expand_region_shrink)
	" maps v and Shift+v in visual mode to run the appropriate
	" methods in the plugin

	"-------------------
	" VIM-AIRLINE CONFIGURATIONS
	" configurations for vim-airline
	set laststatus=2 "puts airline on the 2nd line from the bottom
	" not using a powerline font so comment out next line
	" let g:airline_powerline_fonts = 1
	" end vim-airline stuff
	"let g:Powerline_symbols = 'fancy'
	if !exists('g:airline_symbols')
	  let g:airline_symbols = {}
	endif
	"let g:airline_symbols.space = "\ua0"
	" default theme is dark.vim
	" see also https://github.com/vim-airline/vim-airline-themes
	" for more color choices




	"------
	" INACTIVE SPLITS
	" see the diminactive plugin
	" ToggleDimInactiveWin will change the foreground color of the inactive
	" splits to white. Doesn't seem to affect the terminal.
	" function found here: https://stackoverflow.com/a/17006898/13973

	" create a flag to indicate which way to toggle it
	" let opt_DimInactiveWin=0
	" " we're defining an "Dimmable" syntax highlighting
	" " the highlight line below tells the system what color to use for
	" " "Dimmable" syntax highlighting
	" hi Dimmable ctermfg=235
	" fun! ToggleDimInactiveWin()
	"
	"     if g:opt_DimInactiveWin
	"         " clear the DimWindows group
	"         autocmd! DimWindows
	"         " clears it for all buffers
	"         windo syntax clear Dimmable
	"     else
	"         " define the Dimmable syntax in all buffers
	"         windo syntax region Dimmable start='^' end='$'
	"         " clears it for the _current_ buffer only
	"         syntax clear Dimmable
	"         augroup DimWindows
	"             " when you enter a buffer clear Dimmable syntax
	"             autocmd BufEnter * syntax clear Dimmable
	"             " when you leave a buffer define the inactive syntax
	"             autocmd BufLeave * syntax region Dimmable start='^' end='$'
	"         augroup end
	"     en
	"     let g:opt_DimInactiveWin=!g:opt_DimInactiveWin
	" endfun
	" call ToggleDimInactiveWin()
	"
	"

endif


"-------------------------
" custom functions not yet in plugins
"

