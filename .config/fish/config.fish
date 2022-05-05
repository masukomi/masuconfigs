# Path to Oh My Fish install.
# set -q XDG_DATA_HOME
#   and set -gx OMF_PATH "$XDG_DATA_HOME/omf"
#   or set -gx OMF_PATH "$HOME/.local/share/omf"

# Load Oh My Fish configuration.
# if test -f $OMF_PATH/init.fish
# 	source $OMF_PATH/init.fish
# end
#

set -g theme_nerd_fonts yes

if test -f $HOME/.config/fish/config_work.fish
	source $HOME/.config/fish/config_work.fish
end
if test -f $HOME/.config/fish/secrets.fish
	source $HOME/.config/fish/secrets.fish
end
if test -f $HOME/perl5/perlbrew/etc/perlbrew.fish
	source ~/perl5/perlbrew/etc/perlbrew.fish
end

if test -f /usr/local/share/fish/vendor_completions.d/fd.fish
	source /usr/local/share/fish/vendor_completions.d/fd.fish
end
if test -f /usr/local/share/fish/vendor_completions.d/task.fish
	source /usr/local/share/fish/vendor_completions.d/task.fish
end

set -x EDITOR mvim
set -x BAT_PAGER ""
set -x vmm_use_secure_cookies false
set -x LLVM_HOME /usr/local/opt/llvm
set -x DYLD_LIBRARY_PATH $LLVM_HOME/lib
# set -x RUBY_CONFIGURE_OPTS "--with-openssl-dir="(brew --prefix openssl@1.1)

# Abbreviations
#
# abbr -a gcane 'git commit --amend --no-edit'
# abbr -a gits "git status -uno"
# abbr -a gits 'git status -uno'
# abbr -a tree "broot" # a tree replacement with funky capabilities
abbr -a "brewed?" "is_brewed"
abbr -a be 'bundle exec'
abbr -a ber 'bundle exec rspec --format=documentation'
abbr -a berc 'bundle exec rails console'
abbr -a berd 'bundle exec rails server --debugger'
abbr -a bers 'bundle exec rails server'
# how to generate the keys for ssl: https://gist.github.com/masukomi/937f65c9c42edaeac50a8676a0b8fa8e
abbr -a berss "bundle exec rails server -u puma -b 'ssl://0.0.0.0:9292?key=$HOME/.ssh/server.key&cert=$HOME/.ssh/server.crt&verify_mode=none'"
abbr -a brewed 'is_brewed'
abbr -a build_tags "~/brew/bin/ctags -R --c++-kinds +p --fields +iaS --extra +q --exclude .rsync_cache ."
abbr -a colorsave "script -q /dev/null"
abbr -a doomd 'cd ~/.doom.d'
abbr -a emacsd 'cd ~/.emacs.d'
abbr -a epochmillis "date +%s%N | cut -b1-13"
abbr -a epochtime "date +%s"
abbr -a ga 'git add'
abbr -a gb 'git branch'
abbr -a gbb 'git bisect bad'
abbr -a gbg 'git bisect good'
abbr -a gcm 'git commit -m'
abbr -a gp 'git pick branch'
abbr -a gvv 'git remote -vv'
abbr -a hgrep "history | grep"
abbr -a less "less -R"
abbr -a rca "lcf -p | xargs -I{} sh -c \"echo checking {}; rubocop -A {}\""
abbr -a rt 'rake test'
abbr -a rtnw 'env RUBYOPT=W0 rake test'
abbr -a sqlf "sqlformat --reindent --keywords upper --identifiers lower"
abbr -a top "top -o cpu"
abbr -a unset 'set --erase'

if [ (uname) = "Darwin" ]
	abbr -a ldd "otool -L"
	# because i'm going to forget what the tool is called on OS X when
	# all the docs everywhere say ldd
end

#abbr -a git hub
abbr -a be 'bundle exec'
abbr -a clojure "java -cp ~/workspace/clojure-1.5.1/clojure-1.5.1.jar clojure.main"
#eval (thefuck --alias | tr '\n' ';')
abbr -a gdh "git diff HEAD"
abbr -a gcm "git commit -m"
# because i keep typing too fast and saying its instead of gits
abbr -a its "git status -uno"
abbr -a ag "ag -p ~/.ignore"

abbr -a dc "docker-compose"
abbr -a dce "docker-compose exec"
abbr -a dcu "docker-compose up"
abbr -a dcr "docker-compose run"
abbr -a dcb "docker-compose build"
abbr -a do "docker"
abbr -a berd 'docker-compose exec bin/rspec --format=documentation'

abbr -a sbw 'env SKIP=bad_words'
abbr -a sall 'env SKIP=bad_words,rb_tester,rubocopper'

alias vimr /Applications/VimR.app/Contents/Resources/vimr
abbr -a vfz "mvim (fzf)"
abbr -a which "command -v"
alias :q exit

alias lg lazygit
alias gcurl /usr/local/opt/curl/bin/curl


# Convenience
# I always want the screen cleared before listing my tasks
# abbr -a task "clear; and task"

# PATH
fish_add_path -p -g $HOME/.asdf/shims
# by adding ./bin to the PATH
# ruby developers can skip using "bundle exec"
fish_add_path -g ./bin
fish_add_path -g -p /opt/homebrew/bin
# (which fish | sed "s/\/fish//")
fish_add_path -g . $HOME/bin $HOME/bin/git-scripts /usr/local/bin $PATH
# vvv make python 3 found before macOSs python 2.7
# macOS one is at /usr/local/bin/python
set -x PATH /usr/local/opt/python/libexec/bin $PATH
set brewed_python_version (brew ls --versions python | sed -e "s/python@//" -e "s/ .*//")
fish_add_path -g -a $HOME/Library/Python/$brewed_python_version/bin
fish_add_path -g -a /Applications
fish_add_path -g -a /usr/local/opt/coreutils/libexec/gnubin
fish_add_path -g -a $HOME/.iterm2/
# fish_add_path -g /usr/bin
# fish_add_path -g /bin
# fish_add_path -g /usr/sbin
# fish_add_path -g /sbin
fish_add_path -g -a $HOME/Applications
fish_add_path -g -a $HOME/workspace/gpup
fish_add_path -g -a $HOME/workspace/git_accessories
fish_add_path -g -a $HOME/workspace/gocode/bin
fish_add_path -g -a $HOME/.cargo/bin
fish_add_path -g -a $HOME/.local/bin # haskell stuff installed with Stack
# we're prepending this in case the homebrew version is installed
fish_add_path -g -p $HOME/workspace/private_comments/bin
fish_add_path /usr/local/opt/mongodb-community@4.2/bin


set CELLAR (brew --cellar)
fish_add_path -g -a $CELLAR/chicken/5.0.0/bin
# OYYY WHEN IT COMPLAINS ABOUT libchicken.dylib being missing
#ln -s (brew --prefix chicken)/lib/libchicken.dylib /usr/local/lib/libchicken.dylib
set -x -g JAVA_HOME $CELLAR/openjdk/17.0.2/
fish_add_path -g -a $JAVA_HOME/bin
# newer bash in path
fish_add_path -g -p /opt/homebrew/opt/bash/bin

## Racket
if test -e "/Applications/Racket*"
	fish_add_path -g /Applications/Racket*/bin
end

# Radicle.xyz
fish_add_path -g $HOME/.radicle/bin

## BEGIN GERBIL
# set -x PATH /usr/local/opt/gambit-scheme/current/bin $PATH
# set -x PATH /usr/local/opt/gerbil-scheme/libexec/bin $PATH
#
# set -x GERBIL_HOME /usr/local/opt/gerbil-scheme/libexec
# fish_add_path -g $GERBIL_HOME/bin
## END GERBIL

## BEGIN GO
set -x GOPATH $HOME/workspace/gocode
fish_add_path -g $GOPATH/bin
## END GO

# RACKET
fish_add_path -g /Applications/Racket\ v8.2/bin

# RUST
fish_add_path -g $HOME/.cargo/bin

# Elixir / ERLANG
set -x ERL_AFLAGS "-kernel shell_history enabled"

# these 2 found here
# https://dev.to/andresdotsh/how-to-install-erlang-on-macos-with-asdf-3p1c
set -x CFLAGS "-O2 -g -fno-stack-check"
# set -x KERL_CONFIGURE_OPTIONS=(string join "--disable-hipe --without-javac --with-ssl=" (brew --prefix openssl))
set -x KERL_CONFIGURE_OPTIONS "--disable-hipe --without-javac --with-ssl=/usr/local/opt/openssl@1.1"


set -x GPG_TTY (tty)

# readline
set -gx LDFLAGS "-L/usr/local/opt/readline/lib"
set -gx CPPFLAGS "-I/usr/local/opt/readline/include"
set -gx PKG_CONFIG_PATH "/usr/local/opt/readline/lib/pkgconfig"

# END Enscripten

# START Bob The Fish prompt
# ~/.config/fish/functions/fish_prompt.fish
set -g theme_display_ruby no
set -g theme_nerd_fonts no
set -g theme_powerline_fonts yes
# END Bob the Fish

# rvm invocation
# rvm default
# rbenv invocation
# status --is-interactive; and source (rbenv init -|psub)

test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish

set -x PERLLIB $CELLAR/perl/5.24.0_1/lib/perl5/site_perl/5.24.0 $PERLLIB
eval (direnv hook fish)
set -x PERLLIB /Users/krhodes/perl5/perlbrew/perls/perl-5.24.0/lib/site_perl/5.24.0 $PERLLIB
set -g fish_user_paths "/usr/local/opt/openssl@1.1/bin" $fish_user_paths

set -x USE_FENESTRO true

# setting desired time zones for the tz utility
# https://github.com/oz/tz
# full list of possible ones is here: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
set -x TZ_LIST "US/Pacific,Europe/Paris"


# ASDF
# if you have problems with this the docs are here
# https://asdf-vm.com/guide/getting-started.html
# search for Fish. 3 different ways of handling based on how installed
#echo -e "\nsource "(brew --prefix asdf)"/libexec/asdf.fish" >> ~/.config/fish/config.fish
source (brew --prefix asdf)/libexec/asdf.fish

if test -d (brew --prefix)"/share/fish/vendor_completions.d"
    set -gx fish_complete_path $fish_complete_path (brew --prefix)/share/fish/vendor_completions.d
end

set -x NVM_DIR "$HOME/.nvm"
