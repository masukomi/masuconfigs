function doom
	if test (count $argv) -gt 0
		emacs "$argv" &
	else
		emacs &
	end
	disown
end

