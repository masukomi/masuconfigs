function brewed
	brew ls --versions $1 > /dev/null
	if test $status -eq 0
		# the package is installed
		echo "installed"
	else
		# the package is not installed
		echo "NOT installed"
	end
end

