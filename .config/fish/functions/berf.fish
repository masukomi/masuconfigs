function berf --argument-names 'filename' 'path' -d "runs bundl exec rspec against fbn results"
	set location (fbn "$filename" "$path")
	if test "$location" != ""
		echo "Testing: $location"
		time bundle exec rspec --format=documentation "$location"
	else
		echo "Not Found"
	end
end

