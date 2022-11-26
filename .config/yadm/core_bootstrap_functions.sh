#!/usr/bin/env bash
#
#
function is_installed(){
	hash $1 >/dev/null 2>&1
	# if [ $? -eq 0 ] && [ "$(which go)" != "" ]; then
	if [ $? -eq 0 ]; then
		true
	else
		false
	fi
}

