# Path to Oh My Fish install.
# set -q XDG_DATA_HOME
#   and set -gx OMF_PATH "$XDG_DATA_HOME/omf"
#   or set -gx OMF_PATH "$HOME/.local/share/omf"

# Load Oh My Fish configuration.
# if test -f $OMF_PATH/init.fish
# 	source $OMF_PATH/init.fish
# end
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
set -x vmm_use_secure_cookies false
set -x LLVM_HOME /usr/local/opt/llvm
set -x DYLD_LIBRARY_PATH $LLVM_HOME/lib
# set -x RUBY_CONFIGURE_OPTS "--with-openssl-dir="(brew --prefix openssl@1.1)

# Abbreviations
#
abbr -a less "less -R"
abbr -a gits "git status -uno"
abbr -a top "top -o cpu"
#Compensating for stupidity
abbr -a givm vimr
abbr -a gvmi vimr
#End stupidity...
abbr -a be 'bundle exec'
abbr -a ga 'git add'
abbr -a gits 'git status -uno'
abbr -a gbg 'git bisect good'
abbr -a gbb 'git bisect bad'
abbr -a gb 'git branch'
# abbr -a gcane 'git commit --amend --no-edit'
abbr -a gcm 'git commit -m'
abbr -a epochtime "date +%s"
abbr -a epochmillis "date +%s%N | cut -b1-13"
abbr -a ber 'bundle exec rspec --format=documentation'
abbr -a berc 'bundle exec rails console'
abbr -a bers 'bundle exec rails server'
abbr -a berd 'bundle exec rails server --debugger'
abbr -a build_tags "~/brew/bin/ctags -R --c++-kinds +p --fields +iaS --extra +q --exclude .rsync_cache ."
abbr -a hgrep "history | grep"
abbr -a sqlf "sqlformat --reindent --keywords upper --identifiers lower"

if [ (uname) = "Darwin" ]
	abbr -a ldd "otool -L"
	# because i'm going to forget what the tool is called on OS X when
	# all the docs everywhere say ldd
end

#abbr -a git hub
abbr -a be 'bundle exec'
abbr -a clojure "java -cp ~/workspace/clojure-1.5.1/clojure-1.5.1.jar clojure.main"
#eval (thefuck --alias | tr '\n' ';')
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

abbr -a sbw 'env SKIP=bad_words.sh'
abbr -a sall 'env SKIP=bad_words.sh,x_rb_tester.rb'

alias vimr /Applications/VimR.app/Contents/Resources/vimr
abbr -a vfz "mvim (fzf)"
alias :q exit

alias lg lazygit
alias gcurl /usr/local/opt/curl/bin/curl


# Convenience
# I always want the screen cleared before listing my tasks
# abbr -a task "clear; and task"

# PATH
set -x PATH . $HOME/bin $HOME/bin/git-scripts /usr/local/bin $PATH
# vvv make python 3 found before macOSs python 2.7
# macOS one is at /usr/local/bin/python
set -x PATH /usr/local/opt/python/libexec/bin $PATH
set -x PATH $PATH /Applications
set -x PATH $PATH /usr/local/opt/coreutils/libexec/gnubin
set -x PATH $PATH $HOME/.iterm2/
# set -x PATH $PATH /usr/bin
# set -x PATH $PATH /bin
# set -x PATH $PATH /usr/sbin
# set -x PATH $PATH /sbin
set -x PATH $PATH $HOME/Applications
set -x PATH $PATH $HOME/workspace/gpup
set -x PATH $PATH $HOME/workspace/git_accessories
set -x PATH $PATH $HOME/workspace/gocode/bin
set -x PATH $PATH $HOME/.cargo/bin
set -x PATH $PATH $HOME/.local/bin # haskell stuff installed with Stack

set -x PATH $PATH /usr/local/Cellar/chicken/5.0.0/bin

## Racket
set -x PATH $PATH /Applications/Racket\ v7.7/bin

## BEGIN GERBIL
# set -x PATH /usr/local/opt/gambit-scheme/current/bin $PATH
# set -x PATH /usr/local/opt/gerbil-scheme/libexec/bin $PATH
#
# set -x GERBIL_HOME /usr/local/opt/gerbil-scheme/libexec
# set -x PATH $PATH $GERBIL_HOME/bin
## END GERBIL

## BEGIN GO
set -x GOPATH $HOME/workspace/gocode
set -x GOROOT /usr/local/opt/go/libexec
set -x PATH  $PATH $GOROOT/bin
## END GO

# RACKET
set -x PATH $PATH /Applications/Racket\ v7.7/bin

set -x GPG_TTY (tty)

# readline
set -gx LDFLAGS "-L/usr/local/opt/readline/lib"
set -gx CPPFLAGS "-I/usr/local/opt/readline/include"
set -gx PKG_CONFIG_PATH "/usr/local/opt/readline/lib/pkgconfig"

# END Enscripten

# START Bob The Fish prompt
# ~/.config/fish/functions/fish_prompt.fish
set -g theme_display_ruby no
# END Bob the Fish

# rvm invocation
# rvm default
# rbenv invocation
# status --is-interactive; and source (rbenv init -|psub)

test -e {$HOME}/.iterm2_shell_integration.fish ; and source {$HOME}/.iterm2_shell_integration.fish

set -x PERLLIB /usr/local/Cellar/perl/5.24.0_1/lib/perl5/site_perl/5.24.0 $PERLLIB
eval (direnv hook fish)
set -x PERLLIB /Users/krhodes/perl5/perlbrew/perls/perl-5.24.0/lib/site_perl/5.24.0 $PERLLIB
set -g fish_user_paths "/usr/local/opt/openssl@1.1/bin" $fish_user_paths

set -x USE_FENESTRO true

# setting desired time zones for the tz utility
# https://github.com/oz/tz
# full list of possible ones is here: https://en.wikipedia.org/wiki/List_of_tz_database_time_zones
set -x TZ_LIST "US/Pacific,Europe/Paris"
