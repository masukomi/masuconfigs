function concat
	set delimiter $argv[1]
	echo "delimiter->$delimiter"
	set string ""
	for x in $argv[2..-1]
		set string "$string$delimiter$x"
	end
	echo $string
end

