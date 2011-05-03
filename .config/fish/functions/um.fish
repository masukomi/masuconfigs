function mdless 
	pandoc -s -f markdown -t man $argv[1] | groff -T utf8 -man | less -c
end
function umedit 
	mkdir -p ~/.notes; vim ~/.notes/$argv[1]; 
end 
function um 
	mdless ~/.notes/"$argv[1]" 
end 
function umls 
	ls ~/.notes/ 
end


