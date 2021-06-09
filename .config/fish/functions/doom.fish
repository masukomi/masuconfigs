function doom
	if test (count $argv) -gt 0
		emacs "$argv"  > /dev/null 2>&1 &
	else
		emacs > /dev/null 2>&1 &
	end
	disown
end

