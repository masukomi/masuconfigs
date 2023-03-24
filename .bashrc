# ~/.bashrc: executed by bash(1) for non-login shells.
# see /usr/share/doc/bash/examples/startup-files (in the package bash-doc)
# for examples

PROMPT="$HOME/.config/bash/current_theme.sh"
if [[ -f "$PROMPT" ]]; then
    source "$PROMPT"
else
    echo "no prompt file"
fi
if [[ -e ~/.bash_secrets ]]; then
  source ~/.bash_secrets
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

PATH=$PATH:$HOME/Library/Python/3.9/bin

. "$HOME/.cargo/env"

# >>> conda initialize >>>
# !! Contents within this block are managed by 'conda init' !!
__conda_setup="$('/Users/masukomi/anaconda3/bin/conda' 'shell.bash' 'hook' 2> /dev/null)"
if [ $? -eq 0 ]; then
    eval "$__conda_setup"
else
    if [ -f "/Users/masukomi/anaconda3/etc/profile.d/conda.sh" ]; then
        . "/Users/masukomi/anaconda3/etc/profile.d/conda.sh"
    else
        export PATH="/Users/masukomi/anaconda3/bin:$PATH"
    fi
fi
unset __conda_setup
# <<< conda initialize <<<

export PATH="$HOME/.rbenv/bin:$PATH"
eval "$(rbenv init -)"
