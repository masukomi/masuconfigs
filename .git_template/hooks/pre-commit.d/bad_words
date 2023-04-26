#!/bin/sh

# Modify this
# LIST='list\|of\|words\|splitted\|by\|slash\|and\|pipe'
# NOTE: list is case SENSITIVE (intentionally)
LIST="\.debug\|debugger\|binding.pry\|byebug\|TODO\|alert(\|console.log("


for FILE in `git diff --cached --name-only --diff-filter=ACM` ; do
    # Check if the file contains one of the words in LIST
    if grep $LIST $FILE; then
      echo $FILE." contains \"uncommittable\" words"
      exit 1
    fi
      done
exit
