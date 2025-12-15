
# set -l start (date +%s)


if test -f /usr/local/share/fish/vendor_completions.d/fd.fish
	source /usr/local/share/fish/vendor_completions.d/fd.fish
end
if test -f /usr/local/share/fish/vendor_completions.d/task.fish
	source /usr/local/share/fish/vendor_completions.d/task.fish
end
# if test -f $HOME/workspace/Watson/watson.fish
# 	source $HOME/workspace/Watson/watson.fish
# end


set -x EDITOR doom
set -x BAT_PAGER "" # no paging! Only spew!
set -x vmm_use_secure_cookies false
set -x HOMEBREW_PREFIX /opt/homebrew
set -x XDG_CONFIG_HOME $HOME/.config

# NORMAL (unix) XDG DATA HOME
set -x XDG_DATA_HOME $HOME/.local/share
# APPLE's stupid XDG DATA HOME with it's stupid space
# set -x XDG_DATA_HOME $HOME/Library/Application\ Support
# I dunno what the unix standard is for caches and It's probably best
# to stick with apple's anyway if there's any cleanup stuff that
# happens related to it.
set -x XDG_CACHE_HOME $HOME/Library/Caches

set -x DOOMDIR $XDG_CONFIG_HOME/doom
abbr -a doomd "cd $DOOMDIR"


## LLVM


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
abbr -a emacsd 'cd ~/.config/emacs'
abbr -a epochmillis "date +%s%N | cut -b1-13"
abbr -a epochtime "date +%s"
abbr -a espansod "$HOME/Library/Application\ Support/espanso"
abbr -a ga 'git add'
abbr -a gb 'git branch'
abbr -a gbb 'git bisect bad'
abbr -a gbg 'git bisect good'
abbr -a gcm 'git commit -m'
abbr -a gp 'git pick branch'
abbr -a gvv 'git remote -vv'
abbr -a hgrep "history | grep"
abbr -a lagit "exa -lah --git" # la(h) git
abbr -a less "less -R"
abbr -a raw "script -q /dev/null"
abbr -a rca "lcf -p | xargs -I{} sh -c \"echo checking {}; rubocop -A {}\""
abbr -a rgall "rg --hidden --no-ignore"
abbr -a rt 'rake test'
abbr -a rtnw 'env RUBYOPT=W0 rake test'
abbr -a sat 'stop_and_tag'
abbr -a sqlf "sqlformat --reindent --keywords upper --identifiers lower"
abbr -a top "top -o cpu"
abbr -a unset 'set --erase'
abbr -a unabbr 'abbr --erase'
abbr -a watson-day 'watson log -d --no-pager'
abbr -a ycom 'yadm com -f ~/.local/share/yadm/repo.git/config'

if [ (uname) = "Darwin" ]
	abbr -a ldd "otool -L"
	# because i'm going to forget what the tool is called on OS X when
	# all the docs everywhere say ldd
end

#abbr -a git hub
abbr -a bb 'cd ~/workspace/backup_brain'
abbr -a be 'bundle exec'
# abbr -a clojure "java -cp ~/workspace/clojure-1.5.1/clojure-1.5.1.jar clojure.main"
#eval (thefuck --alias | tr '\n' ';')
abbr -a gdh "git diff HEAD"
abbr -a gcm "git commit -m"
# because i keep typing too fast and saying its instead of gits
abbr -a its "git status -uno"
# abbr -a ag "ag -p ~/.ignore"

# docker-compose for official docker
# fish_add_path -g -a $HOME/.docker/cli-plugins/
# docker-compose for homebrew
fish_add_path -g -a $HOMEBREW_PREFIX/lib/docker/cli-plugins/
abbr -a dc "docker-compose"
abbr -a dce "docker-compose exec"
abbr -a dcu "docker-compose up"
abbr -a dcr "docker-compose run"
abbr -a dcb "docker-compose build"
abbr -a do "docker"
abbr -a dtl "raku -I ~/workspace/TooLoo/lib  ~/workspace/TooLoo/bin/tooloo"

abbr -a berd 'docker-compose exec bin/rspec --format=documentation'

# skip bad words
abbr -a sbw 'LEFTHOOK_EXCLUDE=bad_words'
# skip fun
abbr -a sfun 'LEFTHOOK_EXCLUDE=fun'
# fuck it all...
abbr -a sall 'LEFTHOOK=0'

# hey, local version
abbr -a heyl 'raku -I lib bin/hey'

# nhook = no hook
abbr -a nhook 'LEFTHOOK=0'
# xhook = exclude hook
abbr -a xhook 'LEFTHOOK_EXCLUDE='

# nfm = number format
abbr -a nfm 'numfmt --grouping'

# alias vimr /Applications/VimR.app/Contents/Resources/vimr
abbr -a vfz "mvim (fzf)"
abbr -a which "command -v"
abbr -a zdeps "zef install --deps-only ."
alias :q exit

alias lg lazygit
alias gcurl /usr/local/opt/curl/bin/curl
alias recursor "tput cvvis" # opposite of tput civis
# open a "Quicklook" preview of a file: quicklook <path>
alias quicklook "qlmanage -p"

# "hey" timer functions
alias work "hey stop 2> /dev/null; hey start @ticketsolve"
alias breakfast "hey stop 2> /dev/null; hey start @eating + breakfast"
alias lunch "hey stop 2> /dev/null; hey start @eating + lunch"
alias dogs "hey stop 2> /dev/null; hey start @dogs +walking"
alias bb "hey stop 2> /dev/null; hey start @backup_brain"
alias dg "hey stop 2> /dev/null; hey start @devgood"

# Tailscale
alias tailscale  /Applications/Tailscale.app/Contents/MacOS/Tailscale


# Convenience
# I always want the screen cleared before listing my tasks
# abbr -a task "clear; and task"

# PATH
# by adding ./bin to the PATH
# ruby developers can skip using "bundle exec"
fish_add_path -g ./bin
fish_add_path -g -p $HOMEBREW_PREFIX/bin
fish_add_path -g -p $HOMEBREW_PREFIX/sbin
# (which fish | sed "s/\/fish//")
fish_add_path -g . $HOME/bin $HOME/bin/git-scripts $HOME/bin/git-scripts/hooks /usr/local/bin $PATH
# vvv make python 3 found before macOSs python 2.7
# macOS one is at /usr/local/bin/python
set brewed_python_version (brew ls --versions python | sed -e "s/python@//" -e "s/ .*//")
if test $brewed_python_version
	set -x PATH /usr/local/opt/python/libexec/bin $PATH
	fish_add_path -g -a $HOME/Library/Python/$brewed_python_version/bin
end
fish_add_path -g -a /Applications
fish_add_path -g -p $HOMEBREW_PREFIX/opt/grep/libexec/gnubin
fish_add_path -g -p $HOMEBREW_PREFIX/opt/gnu-getopt/bin
fish_add_path -g -p $HOMEBREW_PREFIX/opt/findutils/libexec/gnubin
fish_add_path -g -a $HOME/.iterm2/
# fish_add_path -g /usr/bin
# fish_add_path -g /bin
# fish_add_path -g /usr/sbin
# fish_add_path -g /sbin
fish_add_path -g -a $HOME/Applications
fish_add_path -g -a $HOME/workspace/git_accessories
fish_add_path -g -a $HOME/workspace/go/bin $HOME/go/bin
fish_add_path -g -a $HOME/.cargo/bin
fish_add_path -g -a $HOME/bin/work
fish_add_path -g -p $HOME/.local/bin # haskell stuff installed with Stack
# we're prepending this in case the homebrew version is installed
fish_add_path /usr/local/opt/mongodb-community@4.2/bin
fish_add_path -g -a $HOME/Library/Python/3.9/bin

fish_add_path -g -a $HOME/workspace/reference/bash/git-fuzzy/bin
set -x -g IDF_PATH $HOME/esp/esp-idf
set -x -g IDF_TOOLS_PATH $HOME/.config/espressif

abbr -a gf 'git fuzzy'

set -x -g RIPGREP_CONFIG_PATH $HOME/.config/ripgrep/config

set CELLAR (brew --cellar)

# BEGIN RAKU
# assumes raku install via rakubrew NOT homebrew
# if test -e ~/.rakubrew/shims/rakudo
	# official instructions:
	~/.rakubrew/bin/rakubrew init Fish | source
	#
	#
	# my raku-bootstrap file pre-generates this file
	# so that we don't have to add the loop time to every shell prompt
	# if test -e $HOME/.config/fish/raku_paths.fish
	# 	source $HOME/.config/fish/raku_paths.fish
	# else
	# 	echo "WARNING: raku_paths.fish not found"
	# 	echo "please regenerate it by running"
	#     echo "~/.config/yadm/raku-bootstrap"
	# end

	# old attempt follows

	# # rakubrew init Fish | source
	# fish_add_path -g -a (rakubrew home)/shims
	# for dir in (find (rakubrew home) -type d -name 'bin')
	# 	fish_add_path -g $dir
	# end
# end

# brew ls --versions rakudo-star > /dev/null
# if test $status -eq 0
# 	for dir in (  find  $CELLAR/rakudo-star/**/*/bin -name bin )
# 		fish_add_path -g -a $dir
# 	end
# end

set -l readline_version (brew list readline --versions | sed -e "s/.* //")
set -x -g CSC_OPTIONS "-I$CELLAR/readline/$readline_version/include -L$CELLAR/readline/$readline_version/lib -Wl,-flat_namespace,-undefined,suppress"
fish_add_path -g -a $CELLAR/chicken/5.0.0/bin
# OYYY WHEN IT COMPLAINS ABOUT libchicken.dylib being missing
#ln -s (brew --prefix chicken)/lib/libchicken.dylib /usr/local/lib/libchicken.dylib
set -x -g JAVA_HOME (ls $CELLAR/openjdk | sed -e "s/\///" | sort | tail -n 1)
fish_add_path -g -a $JAVA_HOME/bin
# newer bash in path
fish_add_path -g -p $HOMEBREW_PREFIX/opt/bash/bin
## Racket
fish_add_path -g /Applications/Racket*/bin

# Radicle.xyz
# fish_add_path -g $HOME/.radicle/bin


# BEGIN JAVA &  Antlr
set -l ANTLRPATH $HOME/workspace/reference/java/antlr/antlr-4.10.1-complete.jar
set -x CLASSPATH ".:$ANTLRPATH:$CLASSPATH"
alias antlr4 "java -Xmx500M -cp \"$ANTLRPATH:\$CLASSPATH\" org.antlr.v4.Tool"
alias grun "java -Xmx500M -cp \"$ANTLRPATH:\$CLASSPATH\" org.antlr.v4.gui.TestRig"
# END JAVA & Antlr

# BEGIN RUBY
set -x OBJC_DISABLE_INITIALIZE_FORK_SAFETY "YES"
# END RUBY

# RUST
fish_add_path -g $HOME/.cargo/bin

# Elixir / ERLANG
# set -x ERL_AFLAGS "-kernel shell_history enabled"

# # these 2 found here
# # https://dev.to/andresdotsh/how-to-install-erlang-on-macos-with-asdf-3p1c
# set -x CFLAGS "-O2 -g -fno-stack-check"
# # set -x KERL_CONFIGURE_OPTIONS=(string join "--disable-hipe --without-javac --with-ssl=" (brew --prefix openssl))
# set -x KERL_CONFIGURE_OPTIONS "--disable-hipe --without-javac --with-ssl=/usr/local/opt/openssl@1.1"
#


set -x GPG_TTY (tty)


# load compiler flags.
# takes ~ 6 seconds to generate this data.
# if it's old, then run the generate_compiler_flags function
if test -f $HOME/.config/fish/compiler_flags.fish
	source $HOME/.config/fish/compiler_flags.fish
else
	echo "generating compiler flags..."
	generate_compiler_flags
	source $HOME/.config/fish/compiler_flags.fish
end

# load the curent theme
source ~/.config/fish/current_theme.fish

# rbenv invocation
if test (command -v rbenv)
	status --is-interactive; and rbenv init - fish | source
	fish_add_path -g -p $HOME/.rbenv/shims
end

test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish

# load/unload environment variables based on pwd
# https://direnv.net/
# looks for .envrc files when you cd and loads them
# similar to the .env gem
eval (direnv hook fish)

# set -x USE_FENESTRO true
# doesn't run in current macos because of cert bs.

# setting desired time zones for the tz utility
# https://github.com/oz/tz
# full list of possible ones is here: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
# set -x TZ_LIST "<timezone>,<alias>;<other tz>,<alias>"
set -x TZ_LIST "US/Pacific,Pacific"


# ASDF
# if you have problems with this the docs are here
# https://asdf-vm.com/guide/getting-started.html
# search for Fish. 3 different ways of handling based on how installed
if test (command -v asdf)
  # fish_add_path -p -g $HOME/.asdf/shims
  source (brew --prefix asdf)/libexec/asdf.fish
end

if test -d (brew --prefix)"/share/fish/vendor_completions.d"
    set -gx fish_complete_path $fish_complete_path (brew --prefix)/share/fish/vendor_completions.d
end

# NVM shit
set -x NVM_DIR "$XDG_CONFIG_HOME/nvm"

# echo "fish config startup time: " (math (date +%s) - $start) "s"

# Setting PATH for Python 3.11
# The original version is saved in /Users/masukomi/.config/fish/config.fish.pysave
set -x PATH "/Library/Frameworks/Python.framework/Versions/3.11/bin" "$PATH"

set -x KINDLE_HOME $HOME/Library/Containers/com.amazon.Kindle/Data/Library/Application\ Support/Kindle/My\ Kindle\ Content


## FINAL OVERRIDES & CUSTOM THINGS

if test -f $HOME/.config/fish/config_work.fish
	source $HOME/.config/fish/config_work.fish
end
if test -f $HOME/.config/fish/secrets.fish
	source $HOME/.config/fish/secrets.fish
end

