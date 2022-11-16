function masuconfig_link --argument-names thing
	set -l thing_path (find ~/bin -name "$thing" | head -n1)
	if not test -n "$thing_path"
		set thing_path (fbn $thing ~/.config/fish/functions/)
	end


	if test -n "$thing_path"
		set -l me (whoami)
		set thing_path (echo $thing_path | sed -e "s/\/Users\/$me\///" )
		set url "https://github.com/masukomi/masuconfigs/blob/master/$thing_path"
		echo $url
		printf $url | pbcopy # printf instead of echo because we don't want the newline
		echo "Copied to clipboard."
	else
		echo "Unable to find $thing in ~/bin or fish functions"
	end

end

