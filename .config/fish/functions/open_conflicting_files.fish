function open_conflicting_files
	git diff --name-status --diff-filter=U | sed -e "s/U[[:blank:]]//g" |\
	xargs -I file gvim file
end

