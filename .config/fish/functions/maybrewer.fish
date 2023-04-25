function maybrewer -a library
	echo "# "(brew info $library | head -n3 | tail -n2)
	echo "maybrew '$library'"
end

