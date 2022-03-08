function fbn --argument-names 'filename' 'path' -d "find by name. Looks for a file by name under the current dir"
	if test "$path" = ""
		find . -name "$filename"
	else
		find $path -name "$filename"
	end
end

