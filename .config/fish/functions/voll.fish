function voll --description="vim open last listed"
	set -l last_file (history --contains "ls " \
						| grep --color=none "^ls" \
						| head -n 1 | sed 's/ls //' \
						| sed -E 's/[[:space:]]+$//')
	gvim $last_file
end
