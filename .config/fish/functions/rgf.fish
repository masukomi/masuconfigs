function rgf --argument-names search_term path --description="rg but only paths returned"
	if test "$path" = ""
		set path "./"
	end
	rg --files-with-matches "$search_term" "$path"
end

