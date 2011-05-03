function gall --description="git add last listed"
	set -l last_file (history --contains "ls " \
						| grep --color=none "^ls" \
						| head -n 1 | sed 's/ls //' \
						| sed -E 's/[[:space:]]+$//')
	git add $last_file
	echo added $last_file
end
