function dprev --description="diff against previous commit"
	if test (count $argv) -gt 0
		set prev "$argv""^"
		git difftool $prev $argv
	else
		git difftool HEAD^ HEAD
	end
end


