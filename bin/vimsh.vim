" file:     vimsh.vim
" purpose:  support file for vimsh, when sourced starts a vimsh buffer
"
" author:   brian m sturk   bsturk@comcast.net,
"                           http://home.comcast.net/~bsturk/vim.html
" created:  12/20/01
" last_mod: 02/13/10
" version:  see vimsh.py
"
" usage:    :so[urce] vimsh.vim

if has("python")

    " Only load vimsh.py once (don't reset variables)
    if !exists("g:vimsh_loaded_python_file")

        pyfile <sfile>:p:h/vimsh.py

        "  If it loaded OK, continue on
        if exists( "g:vimsh_loaded_ok" )
            "  Use ':VimshNewBuf name' to open a new buffer '_name_'
            command! -nargs=1 VimShNewBuf python spawn_buf( "_<args>_" )
        endif

        let g:vimsh_loaded_python_file=1
    endif

    if exists( "g:vimsh_loaded_ok" )
        VimShNewBuf vimsh
    else
        echo "vimsh module did not load, error: '" g:vimsh_load_error "'"
        echo
    endif
else
    echo "vimsh module requires python to be compiled into vim"
    echo
endif
