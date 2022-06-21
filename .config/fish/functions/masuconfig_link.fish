function masuconfig_link --argument-names thing
	set -l thing_path (fbn $thing ~/bin/ | head -n1)
	if not test -n "$thing_path"
		set thing_path (fbn $thing ~/.config/fish/functions/)
	end


	if test -n "$thing_path"
		set -l me (whoami)
		set thing_path (echo $thing_path | sed -e "s/\/Users\/$me\///" )
		echo "https://github.com/masukomi/masuconfigs/blob/master/$thing_path"
	else
		echo "Unable to find $thing in ~/bin or fish functions"
	end

end

