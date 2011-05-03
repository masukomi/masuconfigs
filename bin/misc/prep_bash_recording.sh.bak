#!/bin/bash

# found here: https://news.ycombinator.com/item?id=16911032
# this script preps a bare bash env so that your recordings
# of bash things aren't filled with all your custom hoo-ha 
# littering the screen and distracting from the important stuff.

# Start a new bash shell with a severely filtered environment and no initfile.
if [ -z "$_BFR_RUNNING" ]; then
  env -i \
      _BFR_RUNNING=1 \
      PATH="$PATH" \
      LD_LIBRARY_PATH="$LD_LIBRARY_PATH" \
      TERM="$TERM" \
      SHELL="$SHELL" \
      USER="$USER" \
      HOME="$HOME/bfr-home" \
      LANG="$LANG" \
      bash --init-file "$0" "$@" <&0
  exit $?
else
  unset _BFR_RUNNING
fi

# What remains of this file is the initfile.

USER=user
HOSTNAME=hostname
PS1='\n\[\033[32;45;1m\]\w\[\033[m\]\$ '
eval "$(dircolors -b)"
alias ls='ls --color=auto'
