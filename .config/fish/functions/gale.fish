function gale --description="git add last edited"
	set -l last_file (history --contains "vim " \
						| egrep --color=none "^gvim|^mvim" \
						| head -n 1 | sed -e 's/gvim //' -e 's/mvim //' \
						| sed -E 's/[[:space:]]+$//')
	git add $last_file
	echo added $last_file
end
