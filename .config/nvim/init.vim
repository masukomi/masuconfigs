" masukomi's current .vimrc
scriptencoding utf-8
" set guioptions+=c " Die extra gui warnings die!... or not
set nocompatible
set autoread " reload files when opened externally

" Override default shell stuff
" this is useful in plugins like AsyncRun
set shell=bash
set shellcmdflag=-c

" Install vim-plug if we don't already have it
if empty(glob("~/.vim/autoload/plug.vim"))
    " Ensure all needed directories exist  (Thanks @kapadiamush)
    execute 'mkdir -p ~/.config/nvim/plugged'
    execute 'mkdir -p ~/.config/nvim/autoload'
    " Download the actual plugin manager
    execute '!curl -fLo ~/.vim/autoload/plug.vim https://raw.github.com/junegunn/vim-plug/master/plug.vim'
endif
call plug#begin('~/.config/nvim/plugged')
" NOTE All the plugin names here are actually 
" github repos.
" For example 'gmarik/Vundle.vim'
" is really https://github.com/gmarik/Vundle.vim
" so if you want more info on any of them
" that's how to find it.
"
Plug 'skywind3000/asyncrun.vim'

Plug 'tpope/vim-fugitive'
" Spectacular git integration. If nothing else, check out the GBlame command
" Plug 'godlygeek/tabular'
Plug 'junegunn/vim-easy-align'
" Allows you to align items across multiple lines
" see screencast here: http://vimcasts.org/episodes/aligning-text-with-tabular-vim/
Plug 'tomtom/tcomment_vim'
" allows you to easily toggle lines as commentns or not
"   Visual mode + g + c
Plug 'masukomi/rainbow_parentheses.vim'
" ^^ colorizes matching pairs of {}, [], () so you can see what matches what
Plug 'itspriddle/vim-marked', { 'for': 'markdown' }
" OS X Only - Open the current Markdown buffer in Marked. Supports Marked 1 and 2.
" Marked: http://marked2app.com/
Plug 'chrisbra/Colorizer', { 'for': 'css' }
" A plugin to color colornames and codes
" check out the screenshot: https://github.com/chrisbra/Colorizer#readme
Plug 'bling/vim-airline'
" That pretty piece of awesome at the bottom of every cool vim screen
" the faster successor to Powerline
" https://github.com/bling/vim-airline#readme
Plug 'Yggdroot/indentLine'
let g:indentLine_char = '|'


" let g:indentLine_concealcursor = 'vc' (default 'inc')
" let g:indentLine_conceallevel = 0 "(default 2)
" That shows vertical lines at levels of indentation
" :IndentLinesToggle toggles lines on and off.
Plug 'tpope/vim-surround'
" Surround.vim is all about "surroundings": parentheses, brackets, quotes, XML
" tags, and more. The plugin provides mappings to easily delete, change and add
" such surroundings in pairs.
Plug 'terryma/vim-expand-region'
" is a Vim plugin that allows you to visually select increasingly larger
" regions of text using the same key combination. It is similar to features from
" Plug 'kien/ctrlp.vim'
" a nice way to open files with a fuzzy name / path matching
Plug 'junegunn/fzf', { 'dir': '~/.fzf', 'do': './install --all' }
" another fuzzy finder. requires installation of fzf binary
" see https://github.com/junegunn/fzf for details

Plug 'mileszs/ack.vim'
" useful if you have Ack installed
" Run your favorite search tool from Vim, with an enhanced results list.
" This plugin was designed as a Vim frontend for the Perl module App::Ack. Ack
" can be used as a replacement for 99% of the uses of grep. The plugin allows you
" to run ack from Vim, and shows the results in a split window.

Plug 'craigemery/vim-autotag'
" Automatically discover and "properly" update ctags files on save
" you are using excuberant ctags right? RIGHT?!?!
" http://benoithamelin.tumblr.com/post/15101202004/using-vim-exuberant-ctags-easy-source-navigation
" SEE THE ctags / omnicomplete section below
"
" Plug 'rizzatti/dash.vim'
" What do you mean you don't have Dash installed?!
" enables the :Dash commands
" https://github.com/rizzatti/dash.vim#readme

Plug 'jlanzarotta/bufexplorer'

" LANGUAGE PLUGINS
" Plug 'scrooloose/syntastic'
" a syntax checker which displays results in the gutter
Plug 'masukomi/html_encode_decode'
" Plug 'mustache/vim-mustache-handlebars'
Plug 'fatih/vim-go', { 'for': 'go' }
Plug 'rust-lang/rust.vim', { 'for': 'rust' }
Plug 'rhysd/vim-crystal', { 'for': 'crystal' }
Plug 'dag/vim-fish', { 'for': 'fish' }
Plug 'ElmCast/elm-vim', { 'for' : 'elm' }
"Plug 'Keithbsmiley/swift.vim'
Plug 'kballard/vim-swift', { 'for': 'swift' }
Plug 'roalddevries/yaml.vim', { 'for': 'yaml' }
"Plug 'kchmck/vim-coffee-script'
"Plug 'wlangstroth/vim-racket'
" LANGUAGE ENHANCING PLUGINS
" Plug 'rcyrus/snipmate-snippets-rubymotion'
Plug 'jelera/vim-javascript-syntax', { 'for': 'javascript' }
"Plug 'fousa/vim-flog'
" Plug 'malept/vim-flog'
" Plug 'vrybas/vim-flog', { 'for': 'ruby' }
"Plug 'skammer/vim-ruby-complexity'
" Plug 'wfleming/vim-codeclimate'
"Plug 'airblade/vim-gitgutter'
" ^^^ great but commented out because started having serious issues
"     constantly complaining about not being able to find temp files
"Plug 'scrooloose/syntastic'
" Syntastic is a syntax checking plugin for Vim that runs files through
" external syntax checkers and displays any resulting errors to the user.
Plug 'tpope/vim-endwise'
":if expand("%") == ""|browse confirm w|else|confirm w|endif
"Plug 'masukomi/vim-slime'
"Plug 'Floobits/vim-plugin'
" Plug 'oplatek/Conque-Shell'
"Plug 'sjl/AnsiEsc.vim'
"Plug 'Lokaltog/powerline', {'rtp': 'powerline/bindings/vim/'}
" Plug 'tpope/vim-fireplace', { 'for': 'clojure' }
" There's a REPL in fireplace, but you probably wouldn't have noticed 
" if I hadn't told you. Such is the way with fireplace.vim. By the way, 
" this plugin is for Clojure.

" Markdown
" Plug 'nelstrom/vim-markdown-folding', { 'for': 'markdown' }
" Allows folding of markdown texts based on headings
Plug 'clarke/vim-renumber', { 'for': 'markdown' }
"Plug 'itspriddle/vim-marked'
"Plug 'nelstrom/vim-markdown-folding'
"Plug 'goldfeld/criticmarkup-vim'
Plug 'SidOfc/mkdx'

" All of your Plugins must be added before the following line
"
"
" COLOR SCHEMES / THEMES
"Plug 'duythinht/inori'
" Plug 'kreeger/benlight'


call plug#end()            " required


"------------------------------------------------
" ENABLE DIRECTORY SPECIFIC .vimrc FILES
" (project specific tweaks can be very useful)
set exrc " Enable use of directory-specific .vimrc
set secure " Only run autocommands owned by me

"------------------------------------------------
" PLUGIN SPECIFIC CONFIGURATIONS
" builds on Plugin 'terryma/vim-expand-region'
" for expanding visual selections
 vmap v <Plug>(expand_region_expand)
 vmap <S-v> <Plug>(expand_region_shrink)
" maps v and Shift+v in visual mode to run the appropriate
" methods in the plugin
"------
" FZF
nnoremap <C-p> :<C-u>FZF<CR>
"------
" vim-easy-align
" Start interactive EasyAlign in visual mode (e.g. vipga)
xmap ga <Plug>(EasyAlign)

" Start interactive EasyAlign for a motion/text object (e.g. gaip)
nmap ga <Plug>(EasyAlign)
"------

"------
" Control P (fuzzy file find / open)
let g:ctrlp_map = '<c-p>'
let g:ctrlp_cmd = 'CtrlP'
"------
" html_encode_decode
:cabbr hencode :call HTMLEncode()
:cabbr hdecode :call HTMLDecode()
:cabbr hstrip :%s/<\_.\{-1,\}>//g
" ^^ that isn't for the plugin, it just seemed a good place to put it

"------
" VIM-AIRLINE CONFIGURATIONS
" configurations for vim-airline
set laststatus=2
let g:airline_powerline_fonts = 1
" end vim-airline stuff
"let g:Powerline_symbols = 'fancy'
if !exists('g:airline_symbols')
  let g:airline_symbols = {}
endif
"let g:airline_symbols.space = "\ua0"
"------

"------
" load up RainbowParentheses
" augroup RainbowParentheses
" 	au!
" 	au VimEnter * RainbowParenthesesToggle
" 	au Syntax * RainbowParenthesesLoadRound
" 	au Syntax * RainbowParenthesesLoadSquare
" 	au Syntax * RainbowParenthesesLoadBraces
" augroup END
"------
" vim-flog support
silent exe "g:flog_enable"
" 
" vim-gitgutter
let g:gitgutter_max_signs = 500  " default value

"------
" Change the gutter color in Syntastic
" Plugin 'scrooloose/syntastic'
" hi SignColumn ctermbg=black guibg=#303030
"------

"------------------------------------------------


"------------------------------------------------
" SEARCH MODIFICATIONS
" semakes /~style searches case sensitive only if there is a capital letter in
" the search expression. *~style searches will continue to be consistently
" case-sensitive
set ignorecase
set smartcase
set scrolloff=3

" highlight search terms...
set hlsearch
set incsearch " ... dynamically as they are typed
"------------------------------------------------

"------------------------------------------------
" NAVIGATION MODIFICATIONS
" There are several commands which move the cursor within the line. 
" When you get to the start/end of a line then these commands will 
" fail as you cannot go on. However, many users expect the cursor to 
" be moved onto the previous/next line. Vim allows you to chose which 
" commands will "wrap" the cursor around the line borders. Here I allow 
" the cursor left/right keys as well as the 'h' and 'l' command to do that.
set ww+=<,>,h,l
" making arrow keys wrap across line breaks in normal mode

imap <Left> <C-O><Left>
imap <Right> <C-O><Right>
"------------------------------------------------

"------------------------------------------------
" TYPING / FORMATTING CONFIGURATION
"
" NOTE: at the bottom of this file you'll see a bunch
" of functions that will enable ctrl+c, ctrl+v, ctrl+x, etc
" so that you can cut, copy, paste, save, undo, redo, with the 
" same key combos you use in all your other apps.

set noexpandtab
" gives you tab if you type a tab rather than converting it 
" to something else.
" I use this in conjunction with the TabToggle function 
" (see below). That will let you swap between tab creating tabs
" and spaces so that you can easily conform to the conventions
" of the language you're working in.

"------
" BEGIN INDENTATION
" the following lets you keep indenting or outdenting without reselecting
vnoremap < <gv
vnoremap > >gv

nnoremap <Tab> >>_
nnoremap <S-Tab> <<_
inoremap <S-Tab> <C-D>
vnoremap <Tab> >gv
vnoremap <S-Tab> <gv
" Just select something in visual mode type > to indent, then keep whacking
" it to keep indenting. Same for < and outdenting.

set copyindent
" Copy the structure of the existing lines indent when autoindenting a new line.
set preserveindent
" When changing the indent of the current line, preserve as much of the
" indent structure as possible.
set autoindent
" Copy indent from current line when starting a new line
" END INDENTATION
"------

set softtabstop=0
" fuck softtabs 
set tabstop=4
" tabs display as four spaces
set shiftwidth=4
" shorter version of the above is :set noet ci pi sts=0 sw=4 ts=4
syn on
" syntax highlighting
set number
" show line numbers
set textwidth=80
" text should start to wrap at 80 chars

"set wildmenu=list:longest
"have % match on if/elsif/else/end/opening and closing XML tags and more

" set selection=exclusive
" Make the selection not include the character under the cursor
" so that you it doesn't keep selecting the newline at the end

"set go+=a
" automatically copy visual selections to the clipboard

set hidden
" I don't remember what this does and the docs are... crap but
" thanks to http://items.sjbach.com/319/configuring-vim-right
" I have it set. 




set viminfo='20,<50,s10,h,%
" remember some stuff after quiting vim:
" marks, registers, searches, buffer list


"------
"set autochdir
" current directory is always matching the
" content of the active window. I don't recommend this.
" better to just cd to the root of your project and 
" open the first file from there. Then everything's relative
" to the root of the project.
"------

"------
" remove trailing spaces
function! TrimWhiteSpace()
    %s/\s\+$//e
endfunction
"
" Uncomment this to AUTOMATICALLY remove trailing whitespace
nnoremap <silent> <Leader>rts :call TrimWhiteSpace()<CR>
"------


"------
function! UseTabs(...)
	set noet ci pi sts=0 sw=4 ts=4
	"noet = noexpandtabs = When inserting text do not expand TABs to spaces. 
endfunction
:cabbr ut :call UseTabs()
"------
"------
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
"------

"------
function Border()
	set colorcolumn=80
	highlight colorcolumn guibg=Black
endfunction
:cabbr border :call Border()
" every now and then it goes away and i need to reload it.
"------

"------
" FOLDING
" see http://vimcasts.org/episodes/how-to-fold/
" for info on how to use folding
set foldmethod=syntax
" whenever I use that without foldlevel
" it auto-folds *everything* when I open a file
set foldlevel=1
" tell it to remember the fold levels you last had in each file
autocmd BufWinLeave * if expand("%") != "" | mkview | endif
" autocmd BufWinEnter * if expand("%") != "" | silent loadview | endif
" the if statement is because otherwise you get errors when opening 
" new buffers that aren't attached to a file. ?* is a proposed solution
" that didn't work for me.
"------


"------------------------------------------------
" USER INTERFACE TWEAKS


"------
" CHOOSING A THEME / COLOR SCHEME
" first find one, 
"    go here http://vimcolors.com/
"    or      https://code.google.com/p/vimcolorschemetest/
"            
" Many can be installed with Vundle in which case 
" you would just use a Plugin line in the section above
" and then enable it below
"
" If you CAN'T load it as a Plugin like that you can 
" find the actual color scheme file, put it in 
" ~/.vim/colors/
" In the file ( or the instructions for it ) you should see a line like
" let g:colors_name="anotherdark" That name is the one you want to put 
" on the next line
" colorscheme anotherdark
syntax enable
silent! colorscheme inori_m
"silent! colorscheme anotherdark


"------
" SETTING FONTS
" POWERLINE / Airline FONTS FOUND HERE: 
"    https://github.com/powerline/fonts/tree/master/Inconsolata-g
"set guifont=Menlo\ for\ Powerline:h22
"set guifont=InputMono\ for\ Powerline:h22
" uses the Menlo-Powerline.otf font
"set guifont=Inconsolata-dz\ for\ Powerline:h22
" set guifont=Inconsolata-g\ for\ Powerline:h22
" GuiFont InconsolataForPowerline Nerd Font:h22
"------
"------
" CROSSHAIRS
" set line hilight color
"hi cursorline cterm=none ctermbg=black guibg=Gray14
hi cursorline cterm=none ctermbg=black guibg=#303030
" set column hilight color
" hi cursorcolumn cterm=none ctermbg=black guibg=Gray14
hi cursorcolumn cterm=none ctermbg=black guibg=#303030
" now actually turn it on
set cursorcolumn
set cursorline
" Please see the Border function below.
" The above configures it but every now and then it's missing 
" on some files and I need to reload it. 

"------

"------
" use Ctrl+L to toggle the line number counting method
" either normal, or relative to the current line
" that way you can easily see the number to type after y or d
" without having to actually count
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

"------
" VISUALIZE TABS AND TRAILING SPACES
" this will 
" * hide space characters at the start of lines (if there are characters after them)
" * display space characters at the end of lines
" * display ALL tab characters (regardless of location).
set list
set lcs=tab:Â»_,trail:Â·
highlight SpecialKey ctermfg=8 guifg=DimGrey
" non-unicode version of the above
" set lcs=tab:>-,trail:*
"------

"------
" CREATE A VISUAL MARKER AT 80 COLUMNS
" Useful as a guide to prevent you from making crazy-long lines of code

" if version > 720 " this number doesn't work on all systems... is weird
" honestly I don't know what version this
" came into play but dreamhost has v 7.2
" and it doesn't work on that. ;)
set colorcolumn=80
highlight colorcolumn guibg=Black
highlight ColorColumn guibg=Black
" ^^^ sets the color of the colorcolumn in GUI versions (MacVim / GVim)
" ctermbg is probably what you'd use for non GUI versions 
" sometimes this needs to be colorcolumn sometimes ColorColumn
" not sure what differentiates which version / system needs which.
" for a list of available colors run :help gui-colors
" endif
"------

" DIFF HIGHLIGHTING
" Highlight VCS conflict markers
match ErrorMsg '^\(<\|=\|>\)\{7\}\([^=].\+\)\?$'
"------


"------
" Explore mode - BECAUSE YOU REALLY DON'T NEED NERD TREE
let g:netrw_liststyle=3
" For those of you using the built in Explore mode 
" (:E or :Explore) instead of NerdTree
" puts netrw into a tree style listing
" see help netrw_liststyle for the other 3 options


" and a handy-dandy function to toggle that view
" Toggle Vexplore with Ctrl-E
function! ToggleVExplorer()
  if exists("t:expl_buf_num")
      let expl_win_num = bufwinnr(t:expl_buf_num)
      if expl_win_num != -1
          let cur_win_nr = winnr()
          exec expl_win_num . 'wincmd w'
          close
          exec cur_win_nr . 'wincmd w'
          unlet t:expl_buf_num
      else
          unlet t:expl_buf_num
      endif
  else
      exec '1wincmd w'
      Vexplore
      let t:expl_buf_num = bufnr("%")
  endif
endfunction
map <silent> <C-E> :call ToggleVExplorer()<CR>

let g:netrw_browse_split = 4
let g:netrw_altv = 1
"------



"------
" MacVim / GVim
" if has('gui_running')
" Commented out because this is a meaningless test to nvim because of the 
" server / client architecture
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
" endif

" FULL SCREEN BABY!
function GoFull()
	set fuoptions=maxvert fu
endfunction
:cabbr gof :call GoFull()


" PLUGINS STUFF

autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType javascript call JavaScriptFold()
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags
autocmd FileType php set omnifunc=phpcomplete#CompletePHP
autocmd FileType c set omnifunc=ccomplete#Complete

"------------------------------------------------
" BASIC VIM CONFIGURATION
" store temp files in a central spot
set backupdir=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
set directory=~/.vim-tmp,~/.tmp,~/tmp,/var/tmp,/tmp
" it'll use the first one it finds
" instead of polluting your working directory with 
" temp files


set shellcmdflag=-ic
set shell=/bin/bash\ -i
" when you run shell commands within vim
" you'll expect them to have the same functions and 
" configuration as when you run them on the terminal
" if you use zsh you'll have to figure out the 
" equivalent command ;) 
" more details here: http://stackoverflow.com/a/9014154/13973

"------------------------------------------------
" INTEGRATIONS WITH EXTERNAL APPS
" formd Markdown shortcuts
" see http://www.drbunsen.org/formd-a-markdown-formatting-tool.html
nmap <leader>fr :%! formd -r<CR>
nmap <leader>fi :%! formd -i<CR>
filetype plugin indent on    " required
let g:colorizer_auto_filetype='cdiff'

nmap <Leader>sh           :source ~/bin/vimsh.vim<CR>
"------------------------------------------------


"------------------------------------------------
" LANGUAGE SPECIFIC HACKS
"------
" SCHEME
" If we're in a scheme file, it's gonna be Chicken Scheme
let g:is_chicken=1
setl complete+=,k~/.vim/chicken_scheme_word_list
"------
" RUBY
" ruby complexity plugin
"g:rubycomplexity_enable_at_startup
" Disable "EX Mode"
"map Q <Nop>

" Allow saving of files as sudo when you forget to start vim using sudo.
cmap w!! w !sudo tee > /dev/null %

" PLUGINS STUFF


" CTAGS / OMNICOMPLETION 
" Toggle the Ctags list on the left pannel with F4
" let Tlist_Ctags_Cmd = "/usr/local/bin/ctags"
let Tlist_Ctags_Cmd = "ripper-tags" " used by rails.vim and taglist.vim
let Tlist_WinWidth = 50
map <F4> :TlistToggle<cr>
" map <F5> :!/usr/local/bin/ctags -R --c++-kinds=+p --fields=+iaS --extra=+q --exclude=.git .<CR>
map <F5> :!ripper-tags -R --exclude=.git .<CR>
set tags=./tags;~/workspace
" reload the ctags with f5

" set up omnicompletion
" filetype plugin on    " required but set above 
autocmd FileType ruby,eruby set omnifunc=rubycomplete#Complete
autocmd FileType python set omnifunc=pythoncomplete#Complete
autocmd FileType javascript set omnifunc=javascriptcomplete#CompleteJS
autocmd FileType html set omnifunc=htmlcomplete#CompleteTags
autocmd FileType css set omnifunc=csscomplete#CompleteCSS
autocmd FileType xml set omnifunc=xmlcomplete#CompleteTags
autocmd FileType php set omnifunc=phpcomplete#CompletePHP
autocmd FileType c set omnifunc=ccomplete#Complete
" AutoComplPop has a tendency to freeze
" when you type a . on some things with
" bazillions of methods (like numbers)
let g:acp_behaviorRubyOmniMethodLength = -1


"------------------------------------------------
set conceallevel=0

"------------------------------------------------
" RANDOM FUNCTIONALITY

" uses the ftags file to let fuzzyfinder
" bring up filenames in subdirectories.
" depends on having ~/bin/ftags.sh be run in advance.
"set tags+=ftags


" When you're dealing with a merge conflict
" and you want BOTH sides of it
" run this to take both
" :call TakeBoth()
function! TakeBoth()
	normal /<<<dd/===dd/>>>dd
endfunction
:cabbr conflicted /<<<<<<<\\|>>>>>>>\\|=======
:cabbr fuck /<<<<<<<\\|>>>>>>>\\|=======


function! ViewHTML()
	normal €ýc€ýb:TOhtml:w:!open %:sleep 100m:!rm %€ýc€ýb;€ý,€ý.;
endfunction


" Text Expansions
iab sterr $stderr.puts("XXX")<ESC>bi


" autocmd FileType ruby,python,java autocmd FileWritePre    * :call TrimWhiteSpace()
" autocmd FileType ruby,python,java autocmd FileAppendPre   * :call TrimWhiteSpace()
" autocmd FileType ruby,python,java autocmd FilterWritePre  * :call TrimWhiteSpace()
" autocmd FileType ruby,python,java autocmd BufWritePre     * :call TrimWhiteSpace()

let g:ctrlp_use_caching = 0
if executable('ag')
    set grepprg=ag\ --nogroup\ --nocolor

    let g:ctrlp_user_command = 'ag %s -l --nocolor -g ""'
else
  let g:ctrlp_user_command = ['.git', 'cd %s && git ls-files . -co --exclude-standard', 'find %s -type f']
  let g:ctrlp_prompt_mappings = {
    \ 'AcceptSelection("e")': ['<space>', '<cr>', '<2-LeftMouse>'],
    \ }
endif


set noscrollbind
" should be this way by default but i'm being overly cautious.
set nocursorbind
" when you have split windows scrolling one shouldn't scroll both
" very annoying. don't know what's causing it to be turned on. 

"------------------------------------
" CUT / COPY / PASTE / UNDO / REDO / etc.
" Copied from Cream http://cream.sourceforge.net/
" standard keycombos from Cream


" Cut/Copy/Paste {{{1

" Cut (two mappings)
imap <silent> <C-x>   <Nop>
imap <silent> <S-Del> <Nop>
vmap <silent> <C-x>   :<C-u>call Cream_cut("v")<CR>
vmap <silent> <S-Del> :<C-u>call Cream_cut("v")<CR>

" Copy (two mappings)
imap <silent> <C-c>      <Nop>
imap <silent> <C-Insert> <Nop>
vmap <silent> <C-c>      :<C-u>call Cream_copy("v")<CR>
vmap <silent> <C-Insert> :<C-u>call Cream_copy("v")<CR>

" Paste
imap <silent> <C-v>      x<BS><C-o>:call Cream_paste("i")<CR>
imap <silent> <S-Insert> x<BS><C-o>:call Cream_paste("i")<CR>
vmap <silent> <C-v>           :<C-u>call Cream_paste("v")<CR>
vmap <silent> <S-Insert>      :<C-u>call Cream_paste("v")<CR>

" Undo
imap <silent> <C-z> <C-b>:call Cream_undo("i")<CR>
vmap <silent> <C-z> :<C-u>call Cream_undo("v")<CR>
" Redo
imap <silent> <C-y> <C-b>:call Cream_redo("i")<CR>
vmap <silent> <C-y> :<C-u>call Cream_redo("v")<CR>

" Save (only when changes)
imap <silent> <C-s> <C-o>:call Cream_update("i")<CR>
vmap <silent> <C-s> :<C-u>call Cream_update("v")<CR>

" Cream functions:

function! Cream_cut(mode)
" cut selection to universal clipboard ("+)

	if a:mode == "v"
		normal gv
		normal "+x
	endif
endfunction

function! Cream_copy(mode)
" copy selection to universal clipboard ("+)

	if a:mode == "v"
		normal gv
		normal "+y
		normal gv
	endif
endfunction

function! Cream_paste(mode)
" paste selection from universal clipboard ("+)

	if     a:mode == "v"
		normal gv
		normal "+P
		" correct position
		normal l
		" don't re-select, sizes may differ
	elseif a:mode == "i"
		" fix win32 paste from app
		call setreg('+', @+, 'c')
		let myvirtualedit = &virtualedit
		set virtualedit=all
		normal `^"+gP
		let &virtualedit = myvirtualedit
	endif

endfunction

" Undo and Redo {{{1

function! Cream_undo(mode)
	undo
	if a:mode == "v"
		normal gv
	endif
endfunction

function! Cream_redo(mode)
	redo
	if a:mode == "v"
		normal gv
	endif
endfunction

function! Cream_update(mode)
" save only when changes
" * we don't use the :update command because it can't prompt to SaveAs
"   an un-named file

	if &modified == 1
		call Cream_save()
	endif
	if a:mode == "v"
		" reselect
		normal gv
	endif
endfunction

function! Cream_save()
" save no matter what
	" if new file
	if expand("%") == ""
		if has("browse")
			browse confirm write
		else
			confirm write
		endif
	else
		confirm write
	endif
endfunction

