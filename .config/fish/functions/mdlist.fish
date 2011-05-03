function mdlist --description="converts lines of input to markdown list"
	sed -E "s/^/* /g"
end

