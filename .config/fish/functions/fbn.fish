function fbn --argument-names 'filename' 'path' -d "find by name. Looks for a file by name under the current dir using regexp to match"

	# details on fd and what it takes can be found here
	# https://github.com/sharkdp/fd#readme
	#
	# fd can be installed with homebrew
	# brew install fd
	if test "$path" = ""
		fd "$filename"
	else
		fd $path "$filename"
	end
end

