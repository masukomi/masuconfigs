function macdir --description "make and change (to) directory"
	if mkdir -p $argv[1]
		cd $argv[1]
	else
		echo "error creating directory"
	end
end
