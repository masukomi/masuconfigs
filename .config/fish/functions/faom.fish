function faom --description "Find And Open Migration"
	set FILE_NAME (echo $argv | sed -e 's/\([A-Z]\)/\_\1/g' -e 's/\_//' | tr '[:upper:]' '[:lower:]')
	fao "*$FILE_NAME.rb"
end
