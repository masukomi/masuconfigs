# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

PROMPT="$HOME/.config/bash/current_theme.sh"
if [[ -f "$PROMPT" ]]; then
    echo "$PROMPT exists."
    source "$PROMPT"
else
    echo "no prompt file"
fi


export GPG_TTY=$(tty)
export USERNAME=`id -nu`
export ARCHFLAGS="-arch x86_64"
export EDITOR='vim'
export VISUAL='vim'
export GOPATH=/usr/local/go
export CHICKEN_BUILD=~/workspace/reference/scheme/chicken-4.10.0
#export CHICKEN_DOC_REPOSITORY=

bind '"\e[A": history-search-backward' #up-arrow through history
bind '"\e[B": history-search-forward' #down-arrow through history

