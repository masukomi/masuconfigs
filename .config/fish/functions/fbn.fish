function fbn --argument-names 'filename' -d "find by name. Looks for a file by name under the current dir"
	find . -name "$filename"
end

