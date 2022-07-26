function masuconfig_link --argument-names thing
	set -l thing_path (fbn $thing ~/bin/ | head -n1)
	if not test -n "$thing_path"
		set thing_path (fbn $thing ~/.config/fish/functions/)
	end


	if test -n "$thing_path"
		set -l me (whoami)
		set thing_path (echo $thing_path | sed -e "s/\/Users\/$me\///" )
		set url "https://github.com/masukomi/masuconfigs/blob/master/$thing_path"
		echo $url
		echo $url | pbcopy
		echo "Copied to clipboard."
	else
		echo "Unable to find $thing in ~/bin or fish functions"
	end

end

