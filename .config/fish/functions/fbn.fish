# fbn: find by name
# Usage: fbn <filename-section> [path] [--exec="<command>"]
#
#   If path is not specified it will search in and under the current
#   directory.
#   If exec is specified, any instance of {} will be replaced with the
#   filename.
#
#   example: fbn ".*.rb" --exec="echo foo: {}"
#   produces
#   FILE: foo.rb ---------------------------------
#   foo: foo.rb
#   FILE: bar.rb ---------------------------------
#   bar: bar.rb
#
# Dependencies:
# fd: https://github.com/sharkdp/fd
# sd: https://github.com/chmln/sd
function fbn --argument-names 'filename' 'path' -d "find by name. Looks for a file by name under the current dir using regexp to match"

	# details on fd and what it takes can be found here
	# https://github.com/sharkdp/fd#readme
	#
	# fd can be installed with homebrew
	# brew install fd
	#
	argparse 'exec=' -- $argv

	if test "$path" = "" || test "--exec" = (echo "$path" | sd "(--exec)=.*" '$1')
		if set -q _flag_exec
			for file in (fd --hidden "$filename")
				fbn_exec "$file" "$_flag_exec"
			end
		else
			fd "$filename"
		end
	else
		if set -q _flag_exec
			for file in (fd --hidden "$filename" "$path")
				fbn_exec "$file" "$_flag_exec"
			end
		else
			fd "$filename" "$path"
		end
	end
end

function fbn_exec --argument-names 'file' 'exec'
	echo "FILE: $file --------------------------------------"
	set -l new_command (echo "$exec" | sed -e "s;{};$file;g")
	eval "$new_command"
end

